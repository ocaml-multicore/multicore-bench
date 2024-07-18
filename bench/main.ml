let benchmarks =
  [
    ("Ref with [@poll error]", Bench_ref.run_suite);
    ("Ref with Mutex", Bench_ref_mutex.run_suite);
    ("Atomic", Bench_atomic.run_suite);
    ("Hashtbl", Bench_hashtbl.run_suite);
    ("Queue", Bench_queue.run_suite);
    ("Stack", Bench_stack.run_suite);
    ("Unix", Bench_unix.run_suite);
    ("Atomic incr", Bench_incr.run_suite);
    ("Bounded_q", Bench_bounded_q.run_suite);
  ]

let () = Multicore_bench.Cmd.run ~benchmarks ()
