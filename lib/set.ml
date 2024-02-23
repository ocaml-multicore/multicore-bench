include Stdlib.Set

let ordered_type (type t) (compare : t -> _) =
  (module struct
    type nonrec t = t

    let compare = compare
  end : OrderedType
    with type t = t)

let make (type t) (compare : t -> _) =
  let (module Elt) = ordered_type compare in
  (module Make (Elt) : Stdlib.Set.S with type elt = t)
