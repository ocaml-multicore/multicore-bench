(** This barrier is designed to take a single cache line (or word) and to return
    with the participating domains synchronized as precisely as possible. *)

type t = int Atomic.t

let bits = (Sys.int_size - 1) / 2
let mask = (1 lsl bits) - 1
let one = 1 lsl bits

let make total =
  if total <= 0 || mask < total then invalid_arg "Barrier: out of bounds";
  Atomic.make total |> Multicore_magic.copy_as_padded

let await t =
  let state = Atomic.fetch_and_add t one in
  let total = state land mask in
  if state lsr bits = total - 1 then Atomic.set t (total - (total lsl bits));

  while 0 < Atomic.get t do
    Domain.cpu_relax ()
  done;

  Atomic.fetch_and_add t one |> ignore;
  while Atomic.get t < 0 do
    Domain.cpu_relax ()
  done
