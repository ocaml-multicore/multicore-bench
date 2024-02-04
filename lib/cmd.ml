let run ~benchmarks ?(budgetf = 0.025) ?(filters = []) ?(argv = Sys.argv)
    ?(flush = true) () =
  let budgetf = ref budgetf in
  let filters = ref filters in

  let rec specs =
    [
      ("-budget", Arg.Set_float budgetf, "seconds\t  Budget for a benchmark");
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

  let budgetf = !budgetf in

  if budgetf < 0.0 || 60.0 *. 60.0 < budgetf then
    invalid_arg "budgetf out of range";

  let run (name, fn) =
    let metrics = fn ~budgetf in
    `Assoc [ ("name", `String name); ("metrics", `List metrics) ]
  in

  let filter =
    match !filters with
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
  in

  `Assoc
    [ ("results", `List (benchmarks |> List.filter filter |> List.map run)) ]
  |> Yojson.Safe.pretty_print ~std:true Format.std_formatter;

  if flush then Format.print_flush ()
