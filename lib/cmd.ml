let print_diff base next =
  let open Data in
  Option.pair (Results.parse base) (Results.parse next)
  |> Option.iter @@ fun (base, next) ->
     List.zip_by Benchmark.compare_by_name base next
     |> List.iter @@ fun ((base : Benchmark.t), (next : Benchmark.t)) ->
        Printf.printf "%s:\n" base.name;
        List.zip_by Metric.compare_by_name base.metrics next.metrics
        |> List.iter @@ fun ((base : Metric.t), (next : Metric.t)) ->
           Printf.printf "  %s:\n" base.name;
           if base.trend <> next.trend || base.units <> next.units then
             Printf.printf "    %.2f %s\n" next.value next.units
           else
             let times = next.value /. base.value in
             if
               (next.trend = `Higher_is_better && times < 0.95)
               || (next.trend = `Lower_is_better && 1.05 < times)
             then
               Printf.printf
                 "    %.2f %s = \x1b[1;31m%.2f\x1b\x1b[0;39;49m x %.2f %s\n"
                 next.value next.units times base.value base.units
             else if
               (next.trend = `Higher_is_better && 1.05 < times)
               || (next.trend = `Lower_is_better && times < 0.95)
             then
               Printf.printf
                 "    %.2f %s = \x1b[1;32m%.2f\x1b\x1b[0;39;49m x %.2f %s\n"
                 next.value next.units times base.value base.units
             else
               Printf.printf
                 "    %.2f %s = \x1b[1;33m%.2f\x1b\x1b[0;39;49m x %.2f %s\n"
                 next.value next.units times base.value base.units

let run_benchmark ~budgetf ~debug (name, fn) =
  if debug then
    (* I wish there was a way to tell dune not to capture stderr. *)
    Printf.printf "Running: %s\n%!" name;
  `Assoc [ ("name", `String name); ("metrics", `List (fn ~budgetf)) ]

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

let run ~benchmarks ?(budgetf = 0.025) ?(filters = []) ?(debug = false) ?diff
    ?(argv = Sys.argv) ?(flush = true) () =
  let budgetf = ref budgetf in
  let filters = ref filters in
  let debug = ref debug in
  let diff = ref diff in

  let rec specs =
    [
      ("-budget", Arg.Set_float budgetf, "seconds\t  Budget for a benchmark");
      ( "-debug",
        Arg.Set debug,
        "\t  Print progress information to help debugging" );
      ( "-diff",
        Arg.String (fun path -> diff := Some path),
        "path.json\t  Show diff against specified base results" );
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

  let results =
    `Assoc
      [
        ( "results",
          `List
            (benchmarks
            |> List.filter (build_filter !filters)
            |> List.map (run_benchmark ~debug:!debug ~budgetf:!budgetf)) );
      ]
  in

  begin
    match !diff with
    | None -> Yojson.Safe.pretty_print ~std:true Format.std_formatter results
    | Some fname -> print_diff (Yojson.Safe.from_file fname) results
  end;

  if flush then Format.print_flush ()
