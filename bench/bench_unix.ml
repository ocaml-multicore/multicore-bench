open Multicore_bench

let run_one ~budgetf ~n_domains () =
  let n_bytes = 65536 in

  let init _ =
    let inn, out = Unix.pipe ~cloexec:true () in
    (inn, out, Bytes.create 1)
  in
  let work _ (inn, out, byte) =
    let n = Unix.write out (Bytes.create n_bytes) 0 n_bytes in
    assert (n = n_bytes);
    for _ = 1 to n_bytes do
      let n : int = Unix.read inn byte 0 1 in
      assert (n = 1)
    done;
    Unix.close inn;
    Unix.close out
  in

  let config =
    Printf.sprintf "%d worker%s" n_domains (if n_domains = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~init ~work ()
  |> Times.to_thruput_metrics ~n:(n_bytes * n_domains) ~singular:"blocking read"
       ~config

let run_suite ~budgetf =
  [ 1; 2; 4 ]
  |> List.concat_map @@ fun n_domains ->
     if Sys.win32 || Domain.recommended_domain_count () < n_domains then []
     else run_one ~budgetf ~n_domains ()
