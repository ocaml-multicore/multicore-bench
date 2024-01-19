type t = { inverted : bool; times_per_domain : float array array; runs : int }

let record ~n_domains ~budgetf ?(n_warmups = 3) ?(n_runs_min = 7)
    ?(before = Fun.id) ~init ~work ?(after = Fun.id) () =
  let barrier_init = Barrier.make n_domains in
  let barrier_before = Barrier.make n_domains in
  let barrier_after = Barrier.make n_domains in
  let results =
    Array.init n_domains @@ fun _ ->
    Stack.create () |> Multicore_magic.copy_as_padded
  in
  let budget_used = ref false |> Multicore_magic.copy_as_padded in
  let runs = ref 0 |> Multicore_magic.copy_as_padded in
  Gc.full_major ();
  let budget_start = Mtime_clock.elapsed () in
  let prepare_for_await () =
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
  let main domain_i =
    Domain_local_await.using ~prepare_for_await ~while_running:(fun () ->
        for _ = 1 to n_warmups do
          if domain_i = 0 then begin
            before ();
            Gc.major ()
          end;
          let state = init domain_i in
          Barrier.await barrier_before;
          work domain_i state;
          Barrier.await barrier_after;
          if domain_i = 0 then after ()
        done;
        while !runs < n_runs_min || not !budget_used do
          Barrier.await barrier_init;
          if domain_i = 0 then begin
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
          let state = init domain_i in
          Barrier.await barrier_before;
          let start = Mtime_clock.elapsed () in
          work domain_i state;
          let stop = Mtime_clock.elapsed () in
          Barrier.await barrier_after;
          if domain_i = 0 then after ();
          Stack.push
            (Mtime.Span.to_float_ns (Mtime.Span.abs_diff stop start)
            *. (1. /. 1_000_000_000.0))
            results.(domain_i)
        done)
  in
  let domains =
    Array.init (n_domains - 1) @@ fun domain_i ->
    Domain.spawn @@ fun () -> main (domain_i + 1)
  in
  main 0;
  Array.iter Domain.join domains;
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
