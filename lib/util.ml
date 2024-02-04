let iter_factor =
  let factor b = if b then 10 else 1 in
  factor (64 <= Sys.word_size)
  * factor (Sys.backend_type = Native)
  * factor (1 < Domain.recommended_domain_count ())

let rec alloc ?(batch = 1000) counter =
  let n = Atomic.get counter in
  if n = 0 then 0
  else
    let batch = Int.min n batch in
    if Atomic.compare_and_set counter n (n - batch) then batch
    else alloc ~batch counter

let cross xs ys =
  xs |> List.concat_map @@ fun x -> ys |> List.map @@ fun y -> (x, y)

module Bits = struct
  type t = { mutable bytes : Bytes.t; mutable length : int }

  let create () = { bytes = Bytes.create 1; length = 0 }

  let push t bool =
    let capacity = Bytes.length t.bytes lsl 3 in
    if t.length == capacity then
      t.bytes <- Bytes.extend t.bytes 0 (capacity lsr 3);
    let byte_i = t.length lsr 3 in
    let mask = 1 lsl (t.length land 7) in
    t.length <- t.length + 1;
    let byte = Char.code (Bytes.unsafe_get t.bytes byte_i) in
    let byte = if bool then byte lor mask else byte land lnot mask in
    Bytes.unsafe_set t.bytes byte_i (Char.chr byte)

  let length t = t.length

  let rec iter fn t i =
    let n = t.length in
    if i < n then begin
      let byte = Char.code (Bytes.unsafe_get t.bytes (i lsr 3)) in
      fn (0 <> byte land 1);
      if i + 1 < n then fn (0 <> byte land 2);
      if i + 2 < n then fn (0 <> byte land 4);
      if i + 3 < n then fn (0 <> byte land 8);
      if i + 4 < n then fn (0 <> byte land 16);
      if i + 5 < n then fn (0 <> byte land 32);
      if i + 6 < n then fn (0 <> byte land 64);
      if i + 7 < n then fn (0 <> byte land 128);
      iter fn t (i + 8)
    end

  let iter fn t = iter fn t 0
end

let generate_push_and_pop_sequence ?(state = Random.State.make_self_init ())
    n_msgs =
  let bits = Bits.create () in
  let rec loop length n_push n_pop =
    if 0 < n_push || 0 < n_pop then begin
      let push = Random.State.bool state && 0 < n_push in
      Bits.push bits push;
      loop
        (if push then length + 1 else if 0 < length then length - 1 else length)
        (n_push - Bool.to_int push)
        (n_pop - Bool.to_int ((not push) && 0 < length))
    end
    else length
  in
  let length = loop 0 n_msgs n_msgs in
  assert (length = 0);
  bits
