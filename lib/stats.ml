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
  Float.sqrt (mean_of (Array.map (fun v -> Float.abs (v -. mean) ** 2.) times))

let median_of times =
  Array.sort Float.compare times;
  let n = Array.length times in
  if n land 1 = 0 then (times.((n asr 1) - 1) +. times.(n asr 1)) /. 2.0
  else times.(n asr 1)

let of_times Times.{ inverted; times_per_domain; runs } =
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
  s |> String.split_on_char ' ' |> String.concat " " (* a non-breaking space *)

let to_json ~name ~description ~units t =
  let trend =
    if t.inverted then `String "higher-is-better" else `String "lower-is-better"
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
