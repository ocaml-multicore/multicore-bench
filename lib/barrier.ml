(** This barrier is designed to take a single cache line (or word) and to return
    with the participating domains synchronized as precisely as possible. *)

type t = Obj.t Atomic.t

let bits = (Sys.int_size - 1) / 2
let mask = (1 lsl bits) - 1
let one = 1 lsl bits

let make total : t =
  if total <= 0 || mask < total then invalid_arg "Barrier: out of bounds";
  Atomic.make (Obj.repr total) |> Multicore_magic.copy_as_padded

let rec fad (t : t) (n : int) backoff =
  let before = Atomic.get t in
  if Obj.is_int before then begin
    let state = Obj.obj before in
    let after = Obj.repr (state + n) in
    if Atomic.compare_and_set t before after then state
    else fad t n (Backoff.once backoff)
  end
  else
    let exn, bt = Obj.obj before in
    Printexc.raise_with_backtrace exn bt

let rec set (t : t) (n : int) backoff =
  let before = Atomic.get t in
  if Obj.is_int before then begin
    if not (Atomic.compare_and_set t before (Obj.repr n)) then
      set t n (Backoff.once backoff)
  end
  else
    let exn, bt = Obj.obj before in
    Printexc.raise_with_backtrace exn bt

let get (t : t) : int =
  let before = Atomic.get t in
  if Obj.is_int before then Obj.obj before
  else
    let exn, bt = Obj.obj before in
    Printexc.raise_with_backtrace exn bt

let await (t : t) =
  let state = fad t one Backoff.default in
  let total = state land mask in
  if state lsr bits = total - 1 then
    set t (total - (total lsl bits)) Backoff.default;

  while 0 < get t do
    Domain.cpu_relax ()
  done;

  fad t one Backoff.default |> ignore;
  while get t < 0 do
    Domain.cpu_relax ()
  done

let rec poison t exn bt =
  let before = Atomic.get t in
  if Obj.is_int before then
    let after = Obj.repr (exn, bt) in
    if not (Atomic.compare_and_set t before after) then poison t exn bt
