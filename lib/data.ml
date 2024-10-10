open Option.Syntax

module Trend = struct
  type t = [ `Lower_is_better | `Higher_is_better ]

  let parse =
    Json.as_string >=> function
    | "lower-is-better" -> Some `Lower_is_better
    | "higher-is-better" -> Some `Higher_is_better
    | _ -> None
end

module Metric = struct
  type units = string

  type t = {
    name : string;
    value : float;
    units : units;
    trend : Trend.t;
    description : string;
  }

  let parse =
    (Json.prop "name" >=> Json.as_string
    & Json.prop "value" >=> Json.as_float
    & Json.prop "units" >=> Json.as_string
    & Json.prop "trend" >=> Trend.parse
    & Json.prop "description" >=> Json.as_string)
    >+> fun (name :: value :: units :: trend :: description) ->
    { name; value; units; trend; description }

  let name x = x.name
end

module Benchmark = struct
  type t = { name : string; metrics : Metric.t list }

  let parse =
    (Json.prop "name" >=> Json.as_string
    & Json.prop "metrics" >=> Json.as_list >+> List.filter_map Metric.parse)
    >+> fun (name :: metrics) -> { name; metrics }

  let name x = x.name
end

module Results = struct
  type t = Benchmark.t list

  let parse =
    Json.prop "results" >=> Json.as_list >+> List.filter_map Benchmark.parse
end
