open Multicore_bench
module Queue = Stdlib.Queue

module Bounded_q : sig
  type 'a t

  val create : ?capacity:int -> unit -> 'a t
  val is_empty : 'a t -> bool
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
  val pop_opt : 'a t -> 'a option
end = struct
  type 'a t = {
    mutex : Mutex.t;
    queue : 'a Queue.t;
    capacity : int;
    not_empty : Condition.t;
    not_full : Condition.t;
  }

  let create ?(capacity = Int.max_int) () =
    if capacity < 0 then invalid_arg "negative capacity"
    else
      let mutex = Mutex.create ()
      and queue = Queue.create () |> Multicore_magic.copy_as_padded
      and not_empty = Condition.create ()
      and not_full = Condition.create () in
      { mutex; queue; capacity; not_empty; not_full }
      |> Multicore_magic.copy_as_padded

  let is_empty t =
    Mutex.lock t.mutex;
    let result = Queue.is_empty t.queue in
    Mutex.unlock t.mutex;
    result

  let is_full_unsafe t = t.capacity <= Queue.length t.queue

  let push t x =
    let was_full = ref false in
    Mutex.lock t.mutex;
    match
      while is_full_unsafe t do
        was_full := true;
        Condition.wait t.not_full t.mutex
      done
    with
    | () ->
        Queue.push x t.queue;
        let n = Queue.length t.queue in
        Mutex.unlock t.mutex;
        if n = 1 then Condition.signal t.not_empty;
        if !was_full && n < t.capacity then Condition.signal t.not_full
    | exception exn ->
        Mutex.unlock t.mutex;
        raise exn

  let pop t =
    let was_empty = ref false in
    Mutex.lock t.mutex;
    match
      while Queue.length t.queue = 0 do
        was_empty := true;
        Condition.wait t.not_empty t.mutex
      done
    with
    | () ->
        let n = Queue.length t.queue in
        let elem = Queue.pop t.queue in
        Mutex.unlock t.mutex;
        if n = t.capacity then Condition.signal t.not_full;
        if !was_empty && 1 < n then Condition.signal t.not_empty;
        elem
    | exception exn ->
        Mutex.unlock t.mutex;
        raise exn

  let pop_opt t =
    Mutex.lock t.mutex;
    let n = Queue.length t.queue in
    let elem_opt = Queue.take_opt t.queue in
    Mutex.unlock t.mutex;
    if n = t.capacity then Condition.signal t.not_full;
    elem_opt
end

let run_one_domain ~budgetf ?(n_msgs = 50 * Util.iter_factor) () =
  let t = Bounded_q.create () in

  let op push =
    if push then Bounded_q.push t (ref push) else Bounded_q.pop_opt t |> ignore
  in

  let init _ =
    assert (Bounded_q.is_empty t);
    Util.generate_push_and_pop_sequence n_msgs
  in
  let work _ bits = Util.Bits.iter op bits in

  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config:"one domain"

let run_one ~budgetf ~n_adders ~n_takers ?(n_msgs = 50 * Util.iter_factor) () =
  let n_domains = n_adders + n_takers in

  let t = Bounded_q.create () in

  let n_msgs_to_take = Countdown.create ~n_domains:n_takers () in
  let n_msgs_to_add = Countdown.create ~n_domains:n_adders () in

  let init _ =
    assert (Bounded_q.is_empty t);
    Countdown.non_atomic_set n_msgs_to_take n_msgs;
    Countdown.non_atomic_set n_msgs_to_add n_msgs
  in
  let work i () =
    if i < n_adders then
      let domain_index = i in
      let rec work () =
        let n = Countdown.alloc n_msgs_to_add ~domain_index ~batch:100 in
        if 0 < n then begin
          for i = 1 to n do
            Bounded_q.push t (ref i)
          done;
          work ()
        end
      in
      work ()
    else
      let domain_index = i - n_adders in
      let rec work () =
        let n = Countdown.alloc n_msgs_to_take ~domain_index ~batch:100 in
        if n <> 0 then begin
          for _ = 1 to n do
            ignore (Bounded_q.pop t)
          done;
          work ()
        end
      in
      work ()
  in

  let config =
    let format role n =
      Printf.sprintf "%d %s%s" n role (if n = 1 then "" else "s")
    in
    Printf.sprintf "%s, %s" (format "adder" n_adders) (format "taker" n_takers)
  in

  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_msgs ~singular:"message" ~config

let run_suite ~budgetf =
  run_one_domain ~budgetf ()
  @ (Util.cross [ 1; 2; 4 ] [ 1; 2; 4 ]
    |> List.concat_map @@ fun (n_adders, n_takers) ->
       if Domain.recommended_domain_count () < n_adders + n_takers then []
       else run_one ~budgetf ~n_adders ~n_takers ())
