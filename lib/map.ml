include Stdlib.Map

let make (type t) (compare : t -> _) =
  let (module Elt) = Ordered.make compare in
  (module Make (Elt) : Stdlib.Map.S with type key = t)
