let[@inline never] finally release acquire scope =
  let x = acquire () in
  match scope x with
  | y ->
      release x;
      y
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      release x;
      Printexc.raise_with_backtrace exn bt

external ( let@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
