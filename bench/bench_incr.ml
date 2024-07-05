open Multicore_bench

let run_one ~budgetf ~n_domains ~approach () =
  let counter = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let n_ops = 500 * Util.iter_factor / n_domains in

  let n_ops_todo = Atomic.make 0 |> Multicore_magic.copy_as_padded in

  let init _ = Atomic.set n_ops_todo n_ops in
  let work _ () =
    match approach with
    | `Cas ->
        let rec work () =
          let n = Util.alloc n_ops_todo in
          if n <> 0 then
            let rec loop n =
              if 0 < n then begin
                let v = Atomic.get counter in
                let success = Atomic.compare_and_set counter v (v + 1) in
                loop (n - Bool.to_int success)
              end
              else work ()
            in
            loop n
        in
        work ()
    | `Cas_backoff ->
        let rec work () =
          let n = Util.alloc n_ops_todo in
          if n <> 0 then
            let rec loop backoff n =
              if 0 < n then begin
                let v = Atomic.get counter in
                if Atomic.compare_and_set counter v (v + 1) then
                  loop Backoff.default (n - 1)
                else loop (Backoff.once backoff) n
              end
              else work ()
            in
            loop Backoff.default n
        in
        work ()
    | `Incr ->
        let rec work () =
          let n = Util.alloc n_ops_todo in
          if n <> 0 then
            let rec loop n =
              if 0 < n then begin
                Atomic.incr counter;
                loop (n - 1)
              end
              else work ()
            in
            loop n
        in
        work ()
  in

  let config =
    Printf.sprintf "%s, %d domains"
      (match approach with
      | `Cas -> "CAS"
      | `Cas_backoff -> "CAS with backoff"
      | `Incr -> "Incr")
      n_domains
  in
  Times.record ~budgetf ~n_domains ~init ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"op" ~config

let run_suite ~budgetf =
  Util.cross [ `Cas; `Cas_backoff; `Incr ] [ 1; 2; 4; 8 ]
  |> List.concat_map @@ fun (approach, n_domains) ->
     if Domain.recommended_domain_count () < n_domains then []
     else run_one ~budgetf ~n_domains ~approach ()
