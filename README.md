[API reference](https://ocaml-multicore.github.io/multicore-bench/doc/multicore-bench/Multicore_bench/index.html)
&middot;
[Benchmarks](https://bench.ci.dev/ocaml-multicore/multicore-bench/branch/main?worker=pascal&image=bench.Dockerfile)

# Multicore-bench

Multicore bench is a framework for writing multicore benchmark executables to
run locally on your computer and on
[current-bench](https://github.com/ocurrent/current-bench).

Benchmarking multicore algorithms tends to a require certain amount of setup,
such as spawning domains, synchronizing them before work, timing the work,
collecting the times, and joining domains, that this framework tries to take
care of for you as conveniently as possible. Furthermore, benchmarking multicore
algorithms in OCaml also involves a number of pitfalls related to how the OCaml
runtime works. For example, when only a single domain is running, several
operations provided by the OCaml runtime use specialized implementations that
take advantage of the fact that there is only a single domain running. In most
cases, when trying to benchmark multicore algorithms, you don't actually want to
measure those specialized runtime implementations.

The design of multicore bench is considered **_experimental_**. We are planning
to improve the design along with
[current-bench](https://github.com/ocurrent/current-bench) in the future to
allow more useful benchmarking experience.

## Crash course to [current-bench](https://github.com/ocurrent/current-bench)

Note that, at the time of writing this,
[current-bench](https://github.com/ocurrent/current-bench) is work in progress
and does not accept enrollment for community projects. However, assuming you
have access to it, to run multicore benchmarks with
[current-bench](https://github.com/ocurrent/current-bench) a number of things
need to be setup:

- You will need a [Makefile](Makefile) with a `bench` target at the root of the
  project. The [current-bench](https://github.com/ocurrent/current-bench)
  service will run your benchmarks through that.

- You likely also want to have a [bench.Dockerfile](bench.Dockerfile) and
  [.dockerignore](.dockerignore) at the root of the project. Make sure that the
  Dockerfile is layered such that it will pickup opam updates when desired while
  also avoiding unnecessary work during rebuilds.

- You will also need the benchmarks and that is where this framework may help.
  You can find examples of multicore benchmarks from the
  [Saturn](https://github.com/ocaml-multicore/saturn/tree/main/bench),
  [Kcas](https://github.com/ocaml-multicore/kcas/tree/main/bench), and
  [Picos](https://github.com/ocaml-multicore/picos/tree/main/bench) projects and
  from the [bench](bench) directory of this repository.

For multicore benchmarks you will also need to have
[current-bench](https://github.com/ocurrent/current-bench) configured to use a
multicore machine, which currently needs to be done by the
[current-bench](https://github.com/ocurrent/current-bench) maintainers.

## Example: Benchmarking `Atomic.incr` under contention

Let's look at a simple example with detailed comments of how one might benchmark
`Atomic.incr` under contention.

Note that this example is written here as a
[MDX](https://github.com/realworldocaml/mdx) document or test. Normally you
would write a benchmark as a command line executable and would likely compile it
in release mode with a native compiler.

We first open the
[`Multicore_bench`](https://ocaml-multicore.github.io/multicore-bench/doc/multicore-bench/Multicore_bench/index.html)
module:

```ocaml
# open Multicore_bench
```

This brings into scope multiple modules including
[`Suite`](https://ocaml-multicore.github.io/multicore-bench/doc/multicore-bench/Multicore_bench/Suite/index.html),
[`Util`](https://ocaml-multicore.github.io/multicore-bench/doc/multicore-bench/Multicore_bench/Util/index.html),
[`Times`](https://ocaml-multicore.github.io/multicore-bench/doc/multicore-bench/Multicore_bench/Times/index.html),
and
[`Cmd`](https://ocaml-multicore.github.io/multicore-bench/doc/multicore-bench/Multicore_bench/Cmd/index.html)
that we used below.

Typically one would divide a benchmark executable into benchmark suites for
different algorithms and data structures. To illustrate that pattern, let's
create a module `Bench_atomic` for our benchmarks suite on atomics:

```ocaml
# module Bench_atomic : sig
    (* The entrypoint to a suite is basically a function.  There is a type
       alias for the signature. *)
    val run_suite : Suite.t
  end = struct
    (* [run_one] runs a single benchmark with the given budget and number of
       domains. *)
    let run_one ~budgetf ~n_domains () =
      (* We scale the number of operations using [Util.iter_factor], which
         depends on various factors such as whether we are running on a 32- or
         64-bit machine, using a native or bytecode compiler, and whether we are
         running on multicore OCaml.  The idea is to make it possible to use the
         benchmark executable as a test that can be run even on slow CI
         machines. *)
      let n = 10 * Util.iter_factor in

      (* In this example, [atomic] is the data structure we are benchmarking. *)
      let atomic =
        Atomic.make 0
        |> Multicore_magic.copy_as_padded
        (* We explicitly pad the [atomic] to avoid false sharing.  With false
           sharing measurements are likely to have a lot of noise that makes
           it difficult to get useful results. *)
      in

      (* We store the number of operations to perform in a scalable countdown
         counter.  The idea is that we want all the workers or domains to work
         at the same time as much as possible, because we want to measure
         performance under contention.  So, instead of e.g. simply having each
         domain run a fixed count loop, which could lead to some domains
         finishing well before others, we let the number of operations performed
         by each domain vary. *)
      let n_ops_to_do =
        Countdown.create ~n_domains ()
      in

      (* [init] is called on each domain before [work].  The return value of
         [init] is passed to [work]. *)
      let init _domain_index =
        (* It doesn't matter that we set the countdown counter multiple times.
           We could also use a [before] callback to do setup before [work]. *)
        Countdown.non_atomic_set n_ops_to_do n
      in

      (* [work] is called on each domain and the time it takes is recorded.
         The second argument comes from [init]. *)
      let work domain_index () =
        (* Because we are benchmarking operations that take a very small amount
           of time, we run our own loop to perform the operations.  This has
           pros and cons.  One con is that the loop overhead will be part of the
           measurement, which is something to keep in mind when interpreting the
           results.  One pro is that this gives more flexibility in various
           ways. *)
        let rec work () =
          (* We try to allocate some number of operations to perform. *)
          let n = Countdown.alloc n_ops_to_do ~domain_index ~batch:100 in
          (* If we got zero, then we should stop. *)
          if n <> 0 then begin
            (* Otherwise we perform the operations and try again. *)
            for _=1 to n do
              Atomic.incr atomic
            done;
            work ()
          end
        in
        work ()
      in

      (* [config] is a name for the configuration of the benchmark.  In this
         case we distinguish by the number of workers or domains. *)
      let config =
        Printf.sprintf "%d worker%s" n_domains
          (if n_domains = 1 then "" else "s")
      in

      (* [Times.record] does the heavy lifting to spawn domains and measure
         the time [work] takes on them. *)
      let times = Times.record ~budgetf ~n_domains ~init ~work () in

      (* [Times.to_thruput_metrics] takes the measurements and produces both a
         metric for the time of a single operation and for the total thruput
         over all the domains. *)
      Times.to_thruput_metrics ~n ~singular:"incr" ~config times

    (* [run_suite] runs the benchmarks in this suite with the given budget. *)
    let run_suite ~budgetf =
      (* In this case we run the benchmark with various number of domains. We
         use [concat_map] to collect the results as a flat list of outputs. *)
      [ 1; 2; 4; 8 ]
      |> List.concat_map @@ fun n_domains ->
         run_one ~budgetf ~n_domains ()
  end
module Bench_atomic : sig val run_suite : Suite.t end
```

We then collect all the suites into an association list. The association list
has a name and entry point for each suite:

```ocaml
# let benchmarks = [
    ("Atomic", Bench_atomic.run_suite)
  ]
val benchmarks : (string * Suite.t) list = [("Atomic", <fun>)]
```

Usually the list of benchmarks is in the main module of the benchmark executable
along with an invocation of
[`Cmd.run`](https://ocaml-multicore.github.io/multicore-bench/doc/multicore-bench/Multicore_bench/Cmd/index.html#val-run):

```ocaml non-deterministic
# Cmd.run ~benchmarks ~argv:[||] ()
{
  "results": [
    {
      "name": "Atomic",
      "metrics": [
        {
          "name": "time per incr/1 worker",
          "value": 11.791,
          "units": "ns",
          "trend": "lower-is-better",
          "description": "Time to process one incr",
          "#best": 9.250000000000002,
          "#mean": 12.149960000000002,
          "#median": 11.791,
          "#sd": 1.851061543655424,
          "#runs": 25
        },
        {
          "name": "incrs over time/1 worker",
          "value": 84.81044864727335,
          "units": "M/s",
          "trend": "higher-is-better",
          "description": "Total number of incrs processed",
          "#best": 108.1081081081081,
          "#mean": 84.25129565093134,
          "#median": 84.81044864727335,
          "#sd": 12.911113376793846,
          "#runs": 25
        },
        // ...
      ]
    }
  ]
}
- : unit = ()
```

By default
[`Cmd.run`](https://ocaml-multicore.github.io/multicore-bench/doc/multicore-bench/Multicore_bench/Cmd/index.html#val-run)
interprets command line arguments from
[`Sys.argv`](https://v2.ocaml.org/api/Sys.html#VALargv). Unlike what one would
typically do, we explicitly specify `~argv:[||]`, because this code is being run
through the [MDX](https://github.com/realworldocaml/mdx) tool.

Note that the output above is just a sample. The timings are non-deterministic
and will slightly vary from one run of the benchmark to another even on a single
computer.
