type t = [ `Lower_is_better | `Higher_is_better ]

let to_json = function
  | `Lower_is_better -> `String "lower-is-better"
  | `Higher_is_better -> `String "higher-is-better"
