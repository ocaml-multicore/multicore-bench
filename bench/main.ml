let benchmarks =
  [
    ("Ref with [@poll error]", Bench_ref.run_suite);
    ("Ref with Mutex", Bench_ref_mutex.run_suite);
    ("Atomic", Bench_atomic.run_suite);
    ("Queue", Bench_queue.run_suite);
    ("Stack", Bench_stack.run_suite);
    ("Unix", Bench_unix.run_suite);
  ]

let () = Multicore_bench.Cmd.run ~benchmarks ()
