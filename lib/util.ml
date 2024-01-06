let iter_factor =
  let factor b = if b then 10 else 1 in
  factor (64 <= Sys.word_size)
  * factor (Sys.backend_type = Native)
  * factor (1 < Domain.recommended_domain_count ())

let rec alloc ?(batch = 1000) counter =
  let n = Atomic.get counter in
  if n = 0 then 0
  else
    let batch = Int.min n batch in
    if Atomic.compare_and_set counter n (n - batch) then batch
    else alloc ~batch counter

let cross xs ys =
  xs |> List.concat_map @@ fun x -> ys |> List.map @@ fun y -> (x, y)

let thruput_metrics ~n ~singular ?(plural = singular ^ "s") ~config
    ?(unit_of_time = `ns) ?(rate = `M) times =
  List.concat
    [
      times |> Stats.of_times
      |> Stats.scale (Unit_of_time.to_multiplier unit_of_time /. Float.of_int n)
      |> Stats.to_json
           ~name:(Printf.sprintf "time per %s/%s" singular config)
           ~description:(Printf.sprintf "Time to process one %s" singular)
           ~units:(Unit_of_time.to_mnemonic unit_of_time);
      times |> Times.average |> Times.invert |> Stats.of_times
      |> Stats.scale (Float.of_int n /. Rate.to_divisor rate)
      |> Stats.to_json
           ~name:(Printf.sprintf "%s over time/%s" plural config)
           ~description:(Printf.sprintf "Total number of %s processed" plural)
           ~units:(Rate.to_mnemonic rate);
    ]
