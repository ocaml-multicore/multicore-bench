let make (type t) (compare : t -> _) =
  (module struct
    type nonrec t = t

    let compare = compare
  end : Set.OrderedType
    with type t = t)
