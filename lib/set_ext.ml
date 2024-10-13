let make (type t) (compare : t -> _) =
  let (module Elt) = Ordered.make compare in
  (module Set.Make (Elt) : Set.S with type elt = t)
