let make (type t) (compare : t -> _) =
  (module struct
    type nonrec t = t

    let compare = compare
  end : Stdlib.Set.OrderedType
    with type t = t)
