open Multicore_bench

module Int = struct
  include Int

  let hash = Fun.id
end

module Htbl = Hashtbl.Make (Int)

let run_one_domain ~budgetf ?(n_ops = 400 * Util.iter_factor) ?(n_keys = 1000)
    ~percent_mem ?(percent_add = (100 - percent_mem + 1) / 2)
    ?(prepopulate = true) () =
  let limit_mem = percent_mem in
  let limit_add = percent_mem + percent_add in

  assert (0 <= limit_mem && limit_mem <= 100);
  assert (limit_mem <= limit_add && limit_add <= 100);

  let t = Htbl.create n_keys in

  if prepopulate then
    for _ = 1 to n_keys do
      let value = Random.bits () in
      let key = value mod n_keys in
      Htbl.replace t key value
    done;

  let n_ops = (100 + percent_mem) * n_ops / 100 in

  let n_ops_todo = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ =
    Atomic.set n_ops_todo n_ops;
    Random.State.make_self_init ()
  in
  let work _ state =
    let rec work () =
      let n = Util.alloc n_ops_todo in
      if n <> 0 then
        let rec loop n =
          if 0 < n then
            let value = Random.State.bits state in
            let op = (value asr 20) mod 100 in
            let key = value mod n_keys in
            if op < percent_mem then begin
              begin
                match Htbl.find t key with _ -> () | exception Not_found -> ()
              end;
              loop (n - 1)
            end
            else if op < limit_add then begin
              Htbl.replace t key value;
              loop (n - 1)
            end
            else begin
              Htbl.remove t key;
              loop (n - 1)
            end
          else work ()
        in
        loop n
    in
    work ()
  in

  let config = Printf.sprintf "one domain, %d%% reads" percent_mem in
  Times.record ~budgetf ~n_domains:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let mutex = Mutex.create ()

let run_one ~budgetf ~n_domains ?(n_ops = 400 * Util.iter_factor)
    ?(n_keys = 1000) ~percent_mem ?(percent_add = (100 - percent_mem + 1) / 2)
    ?(prepopulate = true) () =
  let limit_mem = percent_mem in
  let limit_add = percent_mem + percent_add in

  assert (0 <= limit_mem && limit_mem <= 100);
  assert (limit_mem <= limit_add && limit_add <= 100);

  let t = Htbl.create n_keys in

  if prepopulate then
    for _ = 1 to n_keys do
      let value = Random.bits () in
      let key = value mod n_keys in
      Htbl.replace t key value
    done;

  let n_ops = (100 + percent_mem) * n_ops / 100 in

  let n_ops_todo = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ =
    Atomic.set n_ops_todo n_ops;
    Random.State.make_self_init ()
  in
  let work _ state =
    let rec work () =
      let n = Util.alloc n_ops_todo in
      if n <> 0 then
        let rec loop n =
          if 0 < n then
            let value = Random.State.bits state in
            let op = (value asr 20) mod 100 in
            let key = value mod n_keys in
            if op < percent_mem then begin
              Mutex.lock mutex;
              begin
                match Htbl.find t key with _ -> () | exception Not_found -> ()
              end;
              Mutex.unlock mutex;
              loop (n - 1)
            end
            else if op < limit_add then begin
              Mutex.lock mutex;
              Htbl.replace t key value;
              Mutex.unlock mutex;
              loop (n - 1)
            end
            else begin
              Mutex.lock mutex;
              Htbl.remove t key;
              Mutex.unlock mutex;
              loop (n - 1)
            end
          else work ()
        in
        loop n
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s, %d%% reads" n_domains
      (if n_domains = 1 then "" else "s")
      percent_mem
  in
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  ([ 10; 50; 90 ]
  |> List.concat_map @@ fun percent_mem ->
     run_one_domain ~budgetf ~percent_mem ())
  @ (Util.cross [ 10; 50; 90 ] [ 1; 2; 4; 8 ]
    |> List.concat_map @@ fun (percent_mem, n_domains) ->
       run_one ~budgetf ~n_domains ~percent_mem ())
