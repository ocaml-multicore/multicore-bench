open Multicore_bench

let run_one ~budgetf ~n_domains () =
  let block_size = 4096 in
  let n_blocks = 16 in

  let init _ =
    let inn, out = Unix.pipe ~cloexec:true () in
    (inn, out, Bytes.create block_size, Bytes.create 1)
  in
  let work _ (inn, out, block, byte) =
    for _ = 1 to n_blocks do
      let n = Unix.write out block 0 block_size in
      assert (n = block_size);
      for _ = 1 to block_size do
        let n : int = Unix.read inn byte 0 1 in
        assert (n = 1)
      done
    done;
    Unix.close inn;
    Unix.close out
  in

  let config =
    Printf.sprintf "%d worker%s" n_domains (if n_domains = 1 then "" else "s")
  in
  Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~init ~work ()
  |> Times.to_thruput_metrics
       ~n:(block_size * n_blocks * n_domains)
       ~singular:"blocking read" ~config

let run_suite ~budgetf =
  [ 1; 2; 4 ]
  |> List.concat_map @@ fun n_domains ->
     if Sys.win32 || Domain.recommended_domain_count () < n_domains then []
     else run_one ~budgetf ~n_domains ()
