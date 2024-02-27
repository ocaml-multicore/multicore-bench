type t = { inverted : bool; times_per_domain : float array array; runs : int }

let record ~budgetf ~n_domains ?(ensure_multi_domain = true)
    ?(domain_local_await = `Busy_wait) ?(n_warmups = 3) ?(n_runs_min = 7)
    ?(before = Fun.id) ~init ~work ?(after = Fun.id) () =
  let barrier = Barrier.make n_domains in
  let results =
    Array.init n_domains @@ fun _ ->
    Stack.create () |> Multicore_magic.copy_as_padded
  in
  let budget_used = ref false |> Multicore_magic.copy_as_padded in
  let runs = ref 0 |> Multicore_magic.copy_as_padded in
  let exit = ref false in
  let extra_domain =
    if n_domains = 1 && ensure_multi_domain then
      Some
        ( Domain.spawn @@ fun () ->
          while not !exit do
            Domain.cpu_relax ()
          done )
    else None
  in
  Gc.full_major ();
  let budget_start = Mtime_clock.elapsed () in
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
  in
  let start_earliest = Atomic.make Mtime.Span.zero in
  let main domain_i =
    let benchmark () =
      for _ = 1 to n_warmups do
        Barrier.await barrier;
        if domain_i = 0 then begin
          before ();
          Gc.major ()
        end;
        Barrier.await barrier;
        let state = init domain_i in
        Barrier.await barrier;
        work domain_i state;
        Barrier.await barrier;
        if domain_i = 0 then after ()
      done;
      while !runs < n_runs_min || not !budget_used do
        Barrier.await barrier;
        if domain_i = 0 then begin
          Multicore_magic.fenceless_set start_earliest Mtime.Span.zero;
          before ();
          if
            let budget_stop = Mtime_clock.elapsed () in
            let elapsedf =
              Mtime.Span.to_float_ns
                (Mtime.Span.abs_diff budget_stop budget_start)
              *. (1. /. 1_000_000_000.0)
            in
            budgetf < elapsedf
          then budget_used := true;
          incr runs;
          Gc.major ()
        end;
        Barrier.await barrier;
        let state = init domain_i in
        Barrier.await barrier;
        if Multicore_magic.fenceless_get start_earliest == Mtime.Span.zero then begin
          let start_current = Mtime_clock.elapsed () in
          if Multicore_magic.fenceless_get start_earliest == Mtime.Span.zero
          then
            Atomic.compare_and_set start_earliest Mtime.Span.zero start_current
            |> ignore
        end;
        work domain_i state;
        let stop_current = Mtime_clock.elapsed () in
        Barrier.await barrier;
        if domain_i = 0 then after ();
        Stack.push
          (Mtime.Span.to_float_ns
             (Mtime.Span.abs_diff stop_current
                (Multicore_magic.fenceless_get start_earliest))
          *. (1. /. 1_000_000_000.0))
          results.(domain_i)
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
  Array.iter Domain.join domains;
  exit := true;
  Option.iter Domain.join extra_domain;
  let times_per_domain =
    Array.init (Array.length results) @@ fun i ->
    Stack.to_seq results.(i) |> Array.of_seq
  in
  { inverted = false; times_per_domain; runs = !runs }

let average { inverted; times_per_domain; runs } =
  let domains = Array.length times_per_domain in
  let n = Array.length times_per_domain.(0) in
  let times = Array.create_float n in
  for run_i = 0 to n - 1 do
    times.(run_i) <- 0.0;
    for domain_i = 0 to domains - 1 do
      times.(run_i) <- times.(run_i) +. times_per_domain.(domain_i).(run_i)
    done;
    times.(run_i) <- times.(run_i) /. Float.of_int domains
  done;
  { inverted; times_per_domain = [| times |]; runs }

let invert { inverted; times_per_domain; runs } =
  {
    inverted = not inverted;
    times_per_domain =
      Array.map (Array.map (fun v -> 1.0 /. v)) times_per_domain;
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
    Array.fold_left ( +. ) 0.0 times /. Float.of_int (Array.length times)

  let sd_of times mean =
    Float.sqrt
      (mean_of
         (Array.map
            (fun v ->
              let d = v -. mean in
              d *. d)
            times))

  let median_of times =
    Array.sort Float.compare times;
    let n = Array.length times in
    if n land 1 = 0 then (times.((n asr 1) - 1) +. times.(n asr 1)) /. 2.0
    else times.(n asr 1)

  let of_times { inverted; times_per_domain; runs } =
    let domains = Array.length times_per_domain in
    let n = Array.length times_per_domain.(0) in
    let times = Array.create_float n in
    for run_i = 0 to n - 1 do
      times.(run_i) <- 0.0;
      for domain_i = 0 to domains - 1 do
        times.(run_i) <- times.(run_i) +. times_per_domain.(domain_i).(run_i)
      done
    done;
    let mean = mean_of times in
    let sd = sd_of times mean in
    let median = median_of times in
    let best =
      if inverted then Array.fold_left Float.max Float.min_float times
      else Array.fold_left Float.min Float.max_float times
    in
    { mean; sd; median; inverted; best; runs }

  let to_nonbreaking s =
    s |> String.split_on_char ' '
    |> String.concat " " (* a non-breaking space *)

  let to_json ~name ~description ~units t =
    let trend =
      if t.inverted then `String "higher-is-better"
      else `String "lower-is-better"
    in
    [
      `Assoc
        [
          ("name", `String (to_nonbreaking name));
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
           ~name:(Printf.sprintf "time per %s/%s" singular config)
           ~description:(Printf.sprintf "Time to process one %s" singular)
           ~units:(Unit_of_time.to_mnemonic unit_of_time);
      times |> average |> invert |> Stats.of_times
      |> Stats.scale (Float.of_int n /. Unit_of_rate.to_divisor unit_of_rate)
      |> Stats.to_json
           ~name:(Printf.sprintf "%s over time/%s" plural config)
           ~description:(Printf.sprintf "Total number of %s processed" plural)
           ~units:(Unit_of_rate.to_mnemonic unit_of_rate);
    ]
