include Stdlib.List

let default_duplicate _ _ = invalid_arg "duplicate key"
let default_missing _ _ = None

let zip_by (type k) ?(duplicate = default_duplicate)
    ?(missing = default_missing) (compare : k -> _) key_of xs ys =
  let (module M) = Map.make compare in
  let to_map xs =
    xs
    |> fold_left
         (fun m x ->
           m
           |> M.update (key_of x) @@ function
              | None -> Some x
              | Some y -> duplicate x y)
         M.empty
  in
  M.merge
    (fun _ x y ->
      match (x, y) with
      | Some x, Some y -> Some (x, y)
      | Some x, None -> missing `R x
      | None, Some y -> missing `L y
      | None, None -> None)
    (to_map xs) (to_map ys)
  |> M.bindings |> map snd
