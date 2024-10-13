let make (type t) (compare : t -> _) =
  let (module Elt) = Ordered.make compare in
  (module Map.Make (Elt) : Map.S with type key = t)
