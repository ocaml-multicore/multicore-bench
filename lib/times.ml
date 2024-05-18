type t = { inverted : bool; times_per_domain : Float.Array.t array; runs : int }

let with_busy_wait () =
  let open struct
    type state = Init | Released | Awaiting of { mutable released : bool }
  end in
  let state = Atomic.make Init in
  let release () =
    if Multicore_magic.fenceless_get state != Released then
      match Atomic.exchange state Released with
      | Awaiting r -> r.released <- true
      | _ -> ()
  in
  let await () =
    if Multicore_magic.fenceless_get state != Released then
      let awaiting = Awaiting { released = false } in
      if Atomic.compare_and_set state Init awaiting then
        match awaiting with
        | Awaiting r ->
            (* Avoid sleeping *)
            while not r.released do
              Domain.cpu_relax ()
            done
        | _ -> ()
  in
  Domain_local_await.{ release; await }

let wrap _ _ action = action ()

(** ⚠️ This function is written in a very low level manner to avoid memore use,
    allocations, and false sharing as much as possible during a run of [work] as
    those can cause undesirable noise. *)
let record (type a) ~budgetf ~n_domains ?(ensure_multi_domain = true)
    ?(domain_local_await = `Busy_wait) ?(n_warmups = 3) ?(n_runs_min = 7)
    ?(n_runs_max = 1023) ?(before = Fun.id) ~init ?(wrap = wrap) ~work
    ?(after = Fun.id) () =
  Gc.full_major ();
  let open struct
    type shared = {
      barrier : Barrier.t;
      start_earliest : Mtime.Span.t Atomic.t;
      work : int -> a -> unit;
      wrap : int -> a -> (unit -> unit) -> unit;
      results : Float.Array.t Array.t;
      budget_start : Mtime.Span.t;
      before : unit -> unit;
      init : int -> a;
      after : unit -> unit;
      n_warmups : int;
      n_runs_min : int;
      budgetf : float;
      mutable budget_used : bool;
      mutable exit : bool;
      mutable runs : int;
    }
  end in
  let s =
    {
      barrier = Barrier.make n_domains;
      start_earliest =
        Atomic.make Mtime.Span.zero |> Multicore_magic.copy_as_padded;
      work = Multicore_magic.copy_as_padded work;
      wrap;
      results =
        Array.init n_domains (fun _ ->
            Float.Array.create (Int.max n_runs_min n_runs_max));
      budget_start = Mtime_clock.elapsed ();
      before;
      init;
      after;
      n_warmups;
      n_runs_min;
      budgetf;
      budget_used = false;
      exit = false;
      runs = 0;
    }
  in
  let extra_domain =
    if n_domains = 1 && ensure_multi_domain then
      Some
        ( Domain.spawn @@ fun () ->
          while not s.exit do
            Domain.cpu_relax ()
          done )
    else None
  in
  let main i =
    let benchmark () =
      let open struct
        type local = {
          domain_i : int;
          mutable stop_current : Mtime.Span.t;
          mutable state : a;
        }
      end in
      let l =
        Multicore_magic.copy_as_padded
          { domain_i = i; stop_current = Mtime.Span.zero; state = Obj.magic () }
      in
      let doit =
        Multicore_magic.copy_as_padded @@ fun () ->
        Barrier.await s.barrier;
        if Multicore_magic.fenceless_get s.start_earliest == Mtime.Span.zero
        then begin
          let start_current = Mtime_clock.elapsed () in
          if Multicore_magic.fenceless_get s.start_earliest == Mtime.Span.zero
          then
            Atomic.compare_and_set s.start_earliest Mtime.Span.zero
              start_current
            |> ignore
        end;
        s.work l.domain_i l.state;
        l.stop_current <- Mtime_clock.elapsed ()
      in
      (* warmup runs *)
      for _ = 1 to s.n_warmups do
        if l.domain_i = 0 then begin
          Multicore_magic.fenceless_set s.start_earliest Mtime.Span.zero;
          s.before ();
          Gc.major ()
        end;
        Barrier.await s.barrier;
        l.state <- s.init l.domain_i;
        s.wrap l.domain_i l.state doit;
        Barrier.await s.barrier;
        l.state <- Obj.magic ();
        if l.domain_i = 0 then s.after ();
        Barrier.await s.barrier
      done;
      (* timed runs *)
      while s.runs < s.n_runs_min || not s.budget_used do
        if l.domain_i = 0 then begin
          Multicore_magic.fenceless_set s.start_earliest Mtime.Span.zero;
          s.before ();
          Gc.major ()
        end;
        Barrier.await s.barrier;
        l.state <- s.init l.domain_i;
        s.wrap l.domain_i l.state doit;
        Barrier.await s.barrier;
        l.state <- Obj.magic ();
        Float.Array.set s.results.(l.domain_i) s.runs
          (Mtime.Span.to_float_ns
             (Mtime.Span.abs_diff l.stop_current
                (Multicore_magic.fenceless_get s.start_earliest))
          *. (1. /. 1_000_000_000.0));
        l.stop_current <- Mtime.Span.zero;
        Barrier.await s.barrier;
        if l.domain_i = 0 then begin
          s.after ();
          s.runs <- s.runs + 1;
          if
            let budget_stop = Mtime_clock.elapsed () in
            let elapsedf =
              Mtime.Span.to_float_ns
                (Mtime.Span.abs_diff budget_stop s.budget_start)
              *. (1. /. 1_000_000_000.0)
            in
            s.budgetf < elapsedf
            || Float.Array.length s.results.(l.domain_i) <= s.runs
          then s.budget_used <- true
        end;
        Barrier.await s.barrier
      done
    in
    match domain_local_await with
    | `Busy_wait ->
        Domain_local_await.using ~prepare_for_await:with_busy_wait
          ~while_running:benchmark
    | `Neglect -> benchmark ()
  in
  let domains =
    Array.init (n_domains - 1) @@ fun domain_i ->
    Domain.spawn @@ fun () -> main (domain_i + 1)
  in
  main 0;
  s.exit <- true;
  Array.iter Domain.join domains;
  Option.iter Domain.join extra_domain;
  let times_per_domain =
    s.results |> Array.map @@ fun times -> Float.Array.sub times 0 s.runs
  in
  { inverted = false; times_per_domain; runs = s.runs }

let average { inverted; times_per_domain; runs } =
  let domains = Array.length times_per_domain in
  let n = Float.Array.length times_per_domain.(0) in
  let times = Float.Array.create n in
  for run_i = 0 to n - 1 do
    Float.Array.set times run_i 0.0;
    for domain_i = 0 to domains - 1 do
      Float.Array.set times run_i
        (Float.Array.get times run_i
        +. Float.Array.get times_per_domain.(domain_i) run_i)
    done;
    Float.Array.set times run_i
      (Float.Array.get times run_i /. Float.of_int domains)
  done;
  { inverted; times_per_domain = [| times |]; runs }

let invert { inverted; times_per_domain; runs } =
  {
    inverted = not inverted;
    times_per_domain =
      Array.map (Float.Array.map (fun v -> 1.0 /. v)) times_per_domain;
    runs;
  }

module Stats = struct
  type t = {
    mean : float;
    median : float;
    sd : float;
    inverted : bool;
    best : float;
    runs : int;
  }

  let scale factor t =
    {
      t with
      mean = t.mean *. factor;
      median = t.median *. factor;
      sd = t.sd *. factor;
      best = t.best *. factor;
    }

  let mean_of times =
    Float.Array.fold_left ( +. ) 0.0 times
    /. Float.of_int (Float.Array.length times)

  let sd_of times mean =
    Float.sqrt
      (mean_of
         (Float.Array.map
            (fun v ->
              let d = v -. mean in
              d *. d)
            times))

  let median_of times =
    Float.Array.sort Float.compare times;
    let n = Float.Array.length times in
    if n land 1 = 0 then
      (Float.Array.get times ((n asr 1) - 1) +. Float.Array.get times (n asr 1))
      /. 2.0
    else Float.Array.get times (n asr 1)

  let of_times { inverted; times_per_domain; runs } =
    let domains = Array.length times_per_domain in
    let n = Float.Array.length times_per_domain.(0) in
    let times = Float.Array.create n in
    for run_i = 0 to n - 1 do
      Float.Array.set times run_i 0.0;
      for domain_i = 0 to domains - 1 do
        Float.Array.set times run_i
          (Float.Array.get times run_i
          +. Float.Array.get times_per_domain.(domain_i) run_i)
      done
    done;
    let mean = mean_of times in
    let sd = sd_of times mean in
    let median = median_of times in
    let best =
      if inverted then Float.Array.fold_left Float.max Float.min_float times
      else Float.Array.fold_left Float.min Float.max_float times
    in
    { mean; sd; median; inverted; best; runs }

  let to_json ~metric ~config ~description ~units t =
    let trend =
      if t.inverted then `String "higher-is-better"
      else `String "lower-is-better"
    in
    [
      `Assoc
        [
          ("name", `String (Metric.name ~metric ~config));
          ("value", `Float t.median);
          ("units", `String units);
          ("trend", trend);
          ("description", `String description);
          ("#best", `Float t.best);
          ("#mean", `Float t.mean);
          ("#median", `Float t.median);
          ("#sd", `Float t.sd);
          ("#runs", `Int t.runs);
        ];
    ]
end

let to_thruput_metrics ~n ~singular ?(plural = singular ^ "s") ~config
    ?(unit_of_time = `ns) ?(unit_of_rate = `M) times =
  List.concat
    [
      times |> Stats.of_times
      |> Stats.scale (Unit_of_time.to_multiplier unit_of_time /. Float.of_int n)
      |> Stats.to_json
           ~metric:(Printf.sprintf "time per %s" singular)
           ~config
           ~description:(Printf.sprintf "Time to process one %s" singular)
           ~units:(Unit_of_time.to_mnemonic unit_of_time);
      times |> average |> invert |> Stats.of_times
      |> Stats.scale (Float.of_int n /. Unit_of_rate.to_divisor unit_of_rate)
      |> Stats.to_json
           ~metric:(Printf.sprintf "%s over time" plural)
           ~config
           ~description:(Printf.sprintf "Total number of %s processed" plural)
           ~units:(Unit_of_rate.to_mnemonic unit_of_rate);
    ]
