let run ~benchmarks () =
  let budgetf = ref 0.025 in
  let filters = ref [] in

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
         (Filename.basename Sys.argv.(0))
         (benchmarks
         |> List.map (fun (name, _) -> "  " ^ name)
         |> String.concat "\n"));
    exit 1
  in
  Arg.parse specs (fun filter -> filters := filter :: !filters) "";

  let budgetf = !budgetf in

  let run (name, fn) =
    let metrics = fn ~budgetf in
    `Assoc [ ("name", `String name); ("metrics", `List metrics) ]
  in

  let filter =
    match !filters with
    | [] -> Fun.const true
    | filters -> (
        let regexps = filters |> List.map Str.regexp in
        fun (name, _) ->
          regexps
          |> List.exists @@ fun regexp ->
             match Str.search_forward regexp name 0 with
             | _ -> true
             | exception Not_found -> false)
  in

  `Assoc
    [ ("results", `List (benchmarks |> List.filter filter |> List.map run)) ]
  |> Yojson.Safe.pretty_print ~std:true Format.std_formatter
