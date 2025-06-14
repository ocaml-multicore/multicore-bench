open Multicore_bench

module Ref = struct
  type 'a t = 'a ref

  let make = ref
  let get = ( ! )
  let[@poll error] [@inline never] incr x = x := !x + 1

  let[@poll error] [@inline never] compare_and_set x before after =
    !x == before
    && begin
         x := after;
         true
       end

  let[@poll error] [@inline never] exchange x after =
    let before = !x in
    x := after;
    before

  let rec modify ?(backoff = Backoff.default) x f =
    let before = get x in
    let after = f before in
    if not (compare_and_set x before after) then
      modify ~backoff:(Backoff.once backoff) x f
end

type _ op =
  | Get : int op
  | Incr : int op
  | Push_and_pop : int list op
  | Cas_int : int op
  | Xchg_int : int op
  | Swap : (int * int) op

let run_one (type a) ~budgetf ?(n_iter = 500 * Util.iter_factor) (op : a op) =
  let name, extra, (value : a) =
    match op with
    | Get -> ("get", 10, 42)
    | Incr -> ("incr", 1, 0)
    | Push_and_pop -> ("push & pop", 2, [])
    | Cas_int -> ("cas int", 1, 0)
    | Xchg_int -> ("xchg int", 1, 0)
    | Swap -> ("swap", 1, (4, 2))
  in

  let n_iter = n_iter * extra in

  let loc = Ref.make value in

  let init _ = () in
  let work _ () =
    match op with
    | Get ->
        let rec loop i =
          if i > 0 then begin
            let a =
              Ref.get (Sys.opaque_identity loc)
              land Ref.get (Sys.opaque_identity loc)
            and b =
              Ref.get (Sys.opaque_identity loc)
              land Ref.get (Sys.opaque_identity loc)
            and c =
              Ref.get (Sys.opaque_identity loc)
              land Ref.get (Sys.opaque_identity loc)
            and d =
              Ref.get (Sys.opaque_identity loc)
              land Ref.get (Sys.opaque_identity loc)
            in
            loop (i - 8 + (a - b) + (c - d))
          end
        in
        loop n_iter
    | Incr ->
        let rec loop i =
          if i > 0 then begin
            Ref.incr loc;
            Ref.incr loc;
            Ref.incr loc;
            Ref.incr loc;
            Ref.incr loc;
            Ref.incr loc;
            loop (i - 6)
          end
        in
        loop n_iter
    | Push_and_pop ->
        let[@inline] push x = Ref.modify x (fun xs -> 101 :: xs)
        and[@inline] pop x =
          Ref.modify x (function [] -> [] | _ :: xs -> xs)
        in
        let rec loop i =
          if i > 0 then begin
            push loc;
            pop loc |> ignore;
            push loc;
            pop loc |> ignore;
            loop (i - 4)
          end
        in
        loop n_iter
    | Cas_int ->
        let rec loop i =
          if i > 0 then begin
            Ref.compare_and_set loc 0 1 |> ignore;
            Ref.compare_and_set loc 1 0 |> ignore;
            Ref.compare_and_set loc 0 1 |> ignore;
            Ref.compare_and_set loc 1 0 |> ignore;
            Ref.compare_and_set loc 0 1 |> ignore;
            Ref.compare_and_set loc 1 0 |> ignore;
            loop (i - 6)
          end
        in
        loop n_iter
    | Xchg_int ->
        let rec loop i =
          if i > 0 then begin
            Ref.exchange loc 1 |> ignore;
            Ref.exchange loc 0 |> ignore;
            Ref.exchange loc 1 |> ignore;
            Ref.exchange loc 0 |> ignore;
            Ref.exchange loc 1 |> ignore;
            Ref.exchange loc 0 |> ignore;
            loop (i - 6)
          end
        in
        loop n_iter
    | Swap ->
        let[@inline] swap x = Ref.modify x (fun (x, y) -> (y, x)) in
        let rec loop i =
          if i > 0 then begin
            swap loc;
            swap loc;
            swap loc;
            swap loc;
            swap loc;
            swap loc;
            loop (i - 6)
          end
        in
        loop n_iter
  in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_iter ~singular:"op" ~config:name

let run_suite ~budgetf =
  [
    run_one ~budgetf Get;
    run_one ~budgetf Incr;
    run_one ~budgetf Push_and_pop;
    run_one ~budgetf Cas_int;
    run_one ~budgetf Xchg_int;
    run_one ~budgetf Swap;
  ]
  |> List.concat
