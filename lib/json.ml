open Option_ext.Syntax

type t = Yojson.Safe.t

let as_assoc = function `Assoc assoc -> Some assoc | (_ : t) -> None
let prop key = as_assoc >=> List.assoc_opt key
let as_list = function `List list -> Some list | (_ : t) -> None
let as_string = function `String string -> Some string | (_ : t) -> None
let as_float = function `Float float -> Some float | (_ : t) -> None
