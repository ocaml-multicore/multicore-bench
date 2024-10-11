open Multicore_bench

module Int = struct
  include Int

  let hash = Fun.id
end

module Htbl = Hashtbl.Make (Int)

let mutex = Mutex.create ()

let run_one ~budgetf ~n_domains ~use_mutex ?(n_keys = 1000) ~percent_mem
    ?(percent_add = (100 - percent_mem + 1) / 2) ?(prepopulate = true) () =
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

  let n_ops = (if use_mutex then 100 else 400) * Util.iter_factor in
  let n_ops = (100 + percent_mem) * n_ops / 100 in

  let n_ops_todo = Countdown.create ~n_domains () in

  let init _ =
    Countdown.non_atomic_set n_ops_todo n_ops;
    Random.State.make_self_init ()
  in
  let work_no_mutex domain_index state =
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch:100 in
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
  let work_mutex domain_index state =
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch:100 in
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
    let percent_mem = Printf.sprintf "%d%% reads" percent_mem in
    if use_mutex then
      Printf.sprintf "%d worker%s, %s" n_domains
        (if n_domains = 1 then "" else "s")
        percent_mem
    else Printf.sprintf "one domain, %s" percent_mem
  in
  let work = if use_mutex then work_mutex else work_no_mutex in
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  ([ 10; 50; 90 ]
  |> List.concat_map @@ fun percent_mem ->
     run_one ~budgetf ~n_domains:1 ~use_mutex:false ~percent_mem ())
  @ (Util.cross [ 10; 50; 90 ] [ 1; 2; 4; 8 ]
    |> List.concat_map @@ fun (percent_mem, n_domains) ->
       run_one ~budgetf ~n_domains ~use_mutex:true ~percent_mem ())
