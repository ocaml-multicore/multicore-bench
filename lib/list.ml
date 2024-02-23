include Stdlib.List

let zip_by (type t) (compare : t -> _) xs ys =
  let (module S) = Set.make compare in
  let ys = S.of_list ys in
  xs |> filter_map @@ fun x -> S.find_opt x ys |> Option.map @@ fun y -> (x, y)
