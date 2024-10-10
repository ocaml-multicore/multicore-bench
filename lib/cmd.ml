open Data

type output = [ `JSON | `Brief | `Diff of string ]

let worse_colors = [| 196; 197; 198; 199; 200; 201 |]
let better_colors = [| 46; 47; 48; 49; 50; 51 |]

let replace_non_breaking_spaces =
  let a_non_breaking_space = Str.regexp " " in
  Str.global_substitute a_non_breaking_space (fun _ -> " ")

let duplicate kind name x _ =
  failwith
    (Printf.sprintf "Duplicate %s: %s" kind
       (name x |> replace_non_breaking_spaces))

let print_diff base next =
  List.zip_by
    ~duplicate:(duplicate "benchmark" Benchmark.name)
    String.compare Benchmark.name base next
  |> List.iter @@ fun ((base : Benchmark.t), (next : Benchmark.t)) ->
     Printf.printf "%s:\n" base.name;
     let zipped =
       List.zip_by
         ~duplicate:(duplicate "metric" Metric.name)
         String.compare Metric.name base.metrics next.metrics
     in
     let extreme_of join trend =
       List.fold_left
         (fun acc ((base : Metric.t), (next : Metric.t)) ->
           if trend <> base.trend || trend <> next.trend then acc
           else join acc (next.value /. base.value))
         1.0 zipped
     in
     let min_higher = extreme_of Float.min `Higher_is_better in
     let max_higher = extreme_of Float.max `Higher_is_better in
     let min_lower = extreme_of Float.min `Lower_is_better in
     let max_lower = extreme_of Float.max `Lower_is_better in
     zipped
     |> List.iter @@ fun ((base : Metric.t), (next : Metric.t)) ->
        Printf.printf "  %s:\n" base.name;
        if
          base.trend <> next.trend || base.units <> next.units
          || Float.equal base.value next.value
        then Printf.printf "    %.2f %s\n" next.value next.units
        else
          let times = next.value /. base.value in
          let colors, extreme =
            if next.trend = `Higher_is_better then
              if times < 1.0 then (worse_colors, min_higher)
              else (better_colors, max_higher)
            else if 1.0 < times then (worse_colors, max_lower)
            else (better_colors, min_lower)
          in
          let range = Float.abs (extreme -. 1.0) in
          let color =
            colors.(Float.to_int
                      (Float.round
                         (Float.of_int (Array.length colors - 1)
                         *. Float.abs (extreme -. times)
                         /. range)))
          in
          Printf.printf
            "    %.2f %s = \x1b[1;38;5;%dm%.2f\x1b\x1b[0;39;49m x %.2f %s\n"
            next.value next.units color times base.value base.units

let run_benchmark ~budgetf ~debug (name, fn) =
  if debug then
    (* I wish there was a way to tell dune not to capture stderr. *)
    Printf.printf "Running: %s\n%!" name;
  `Assoc [ ("name", `String name); ("metrics", `List (fn ~budgetf)) ]

let name_of = function
  | `Assoc (("name", `String name) :: _) -> name
  | _ -> failwith "bug"

let build_filter = function
  | [] -> Fun.const true
  | filters -> begin
      let regexps = filters |> List.map Str.regexp in
      fun (name, _) ->
        regexps
        |> List.exists @@ fun regexp ->
           match Str.search_forward regexp name 0 with
           | _ -> true
           | exception Not_found -> false
    end

let shuffle xs =
  let xs = Array.of_list xs in
  let state = Random.State.make_self_init () in
  let n = Array.length xs in
  for i = 0 to n - 2 do
    let j = Random.State.int state (n - i) + i in
    let t = xs.(i) in
    xs.(i) <- xs.(j);
    xs.(j) <- t
  done;
  Array.to_list xs

let run ~benchmarks ?(budgetf = 0.025) ?(filters = []) ?(debug = false)
    ?(output = `JSON) ?(argv = Sys.argv) ?(flush = true) ?(randomize = true) ()
    =
  let budgetf = ref budgetf in
  let filters = ref filters in
  let debug = ref debug in
  let output = ref output in
  let randomize = ref randomize in

  let rec specs =
    [
      ("-budget", Arg.Set_float budgetf, "seconds\t  Budget for a benchmark");
      ( "-debug",
        Arg.Set debug,
        "\t  Print progress information to help debugging" );
      ( "-diff",
        Arg.String (fun path -> output := `Diff path),
        "path.json\t  Show diff against specified base results" );
      ( "-brief",
        Arg.Unit (fun () -> output := `Brief),
        "\t  Show brief human readable results." );
      ("-help", Unit help, "\t  Show this help message");
      ("--help", Unit help, "\t  Show this help message");
    ]
  and help () =
    Arg.usage (Arg.align specs)
      (Printf.sprintf
         "\n\
          Usage: %s <option>* filter*\n\n\
          The filters are regular expressions for selecting benchmarks to run.\n\n\
          Benchmarks:\n\n\
          %s\n\n\
          Options:\n"
         (Filename.basename argv.(0))
         (benchmarks
         |> List.map (fun (name, _) -> "  " ^ name)
         |> String.concat "\n"));
    exit 1
  in
  Arg.parse_argv argv specs (fun filter -> filters := filter :: !filters) "";

  if !budgetf < 0.0 || 60.0 *. 60.0 < !budgetf then
    invalid_arg "budgetf out of range";

  let base_results =
    match !output with
    | `Diff fname -> begin
        match Results.parse (Yojson.Safe.from_file fname) with
        | None -> []
        | Some results -> results
      end
    | `JSON | `Brief -> []
  in

  let benchmark_jsons =
    benchmarks
    |> List.filter (build_filter !filters)
    |> begin
         match base_results with
         | [] -> Fun.id
         | results ->
             let (module S) = Set.make String.compare in
             let names = results |> List.map Benchmark.name |> S.of_list in
             List.filter (fun (name, _) -> S.mem name names)
       end
    |> (if !randomize then shuffle else Fun.id)
    |> List.map (run_benchmark ~debug:!debug ~budgetf:!budgetf)
    |> List.sort @@ fun l r -> String.compare (name_of l) (name_of r)
  in

  let results_json = `Assoc [ ("results", `List benchmark_jsons) ] in
  let results =
    lazy
      (match Results.parse results_json with
      | None -> []
      | Some results -> results)
  in

  begin
    match !output with
    | `JSON ->
        Yojson.Safe.pretty_print ~std:true Format.std_formatter results_json
    | `Brief -> print_diff (Lazy.force results) (Lazy.force results)
    | `Diff _ -> print_diff base_results (Lazy.force results)
  end;

  if flush then Format.print_flush ()
