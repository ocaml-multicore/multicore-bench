type t = Yojson.Safe.t

let to_nonbreaking s =
  s |> String.split_on_char ' ' |> String.concat " " (* a non-breaking space *)

let name ~metric ~config = to_nonbreaking (metric ^ "/" ^ config)

let make ~metric ~config ?units ?trend ?description
    (value : [< `Float of float ]) =
  let[@inline] ( @: ) x_opt xs =
    match x_opt with None -> xs | Some x -> x :: xs
  in
  `Assoc
    (Some ("name", `String (name ~metric ~config))
    @: Some ("value", (value :> t))
    @: Option.map (fun units -> ("units", `String units)) units
    @: Option.map (fun trend -> ("trend", Trend.to_json trend)) trend
    @: Option.map
         (fun description -> ("description", `String description))
         description
    @: [])
