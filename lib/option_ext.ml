let pair x y = match (x, y) with Some x, Some y -> Some (x, y) | _ -> None

module Syntax = struct
  let ( & ) l r x =
    match l x with
    | None -> None
    | Some l -> begin
        match r x with None -> None | Some r -> Some Infix_pair.(l :: r)
      end

  let ( let* ) = Option.bind
  let ( >>= ) = Option.bind
  let ( >=> ) f g x = f x >>= g
  let ( let+ ) x f = Option.map f x
  let ( >>+ ) = ( let+ )
  let ( >+> ) f g x = f x >>+ g
  let pure = Option.some
  let ( and* ) = pair
  let ( and+ ) = pair
end
