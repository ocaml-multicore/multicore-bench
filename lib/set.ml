include Stdlib.Set

let make (type t) (compare : t -> _) =
  let (module Elt) = Ordered.make compare in
  (module Make (Elt) : Stdlib.Set.S with type elt = t)
