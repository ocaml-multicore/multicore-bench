(** Multicore bench is a framework for writing multicore benchmark executables
    to run on {{:https://github.com/ocurrent/current-bench}current-bench}.

    To use the framework one typically opens it

    {[
      open Multicore_bench
    ]}

    which brings a number of submodules into scope. *)

module Metric : sig
  (** Dealing with benchmark metrics. *)

  type t
  (** Represents a metric. *)
end

module Unit_of_rate : sig
  (** Dealing with units of rate. *)

  type t =
    [ `_1  (** 1/s *)
    | `k  (** 10{^ 3}/s or k/s *)
    | `M  (** 10{^ 6}/s or M/s *)
    | `G  (** 10{^ 9}/s or G/s *) ]
  (** Represents a unit of rate, i.e. how many per second. *)

  val to_divisor : t -> float
  (** [to_divisor t] converts the unit of rate [t] to a divisor. *)

  val to_mnemonic : t -> string
  (** [to_mnemonic t] returns a human readable mnemonic for the unit of rate
      [t]. *)
end

module Unit_of_time : sig
  (** Dealing with units of time. *)

  type t =
    [ `s  (** seconds *)
    | `ms  (** milliseconds *)
    | `mus  (** microseconds *)
    | `ns  (** nanoseconds *) ]
  (** Represents a unit of time. *)

  val to_multiplier : t -> float
  (** [to_multiplier t] converts the unit of time [t] to a multiplier. *)

  val to_mnemonic : t -> string
  (** [to_mnemonic t] returns a human readable mnemonic for the unit of time
      [t]. *)
end

module Times : sig
  (** Recording timings of benchmarks running on multiple domains in parallel
      and producing metrics from the recorded timings. *)

  type t
  (** Represents a record of elapsed times of multiple runs of a benchmark
      running on multiple domains. *)

  val record :
    budgetf:float ->
    n_domains:int ->
    ?ensure_multi_domain:bool ->
    ?domain_local_await:[< `Busy_wait | `Neglect > `Busy_wait ] ->
    ?n_warmups:int ->
    ?n_runs_min:int ->
    ?before:(unit -> unit) ->
    init:(int -> 's) ->
    work:(int -> 's -> unit) ->
    ?after:(unit -> unit) ->
    unit ->
    t
  (** [record ~budgetf ~n_domains ~init ~work ()] essentially repeatedly runs
      [work i (init i)] on specified number of domains, [i ∊ [0, n_domains-1]],
      and records the times that calls of [work] take.  The calls of [work] are
      synchronized to start as simultaneously as possible.

      Optional arguments:

      - [~ensure_multi_domain]: Whether to run an extra busy untimed domain when
        [n_domains] is [1].  Doing so prevents the OCaml runtime from using
        specialized runtime implementations.  Defaults to [true].

      - [~domain_local_await]: Specifies whether and how to configure
        {{:https://github.com/ocaml-multicore/domain-local-await/}domain-local-await}
        or DLA.  [`Neglect] does not reconfigure DLA.  [`Busy_wait] configures
        DLA to use a busy-wait implementation, which prevents domains from going
        to sleep.  Defaults to [`Busy_wait].

      - [~n_warmups]: Specifies the number of warmup runs to perform before the
        actual measurements.  Defaults to [3].

      - [~n_runs_min]: Specifies the minimum number of timed runs.  The upper
        bound is determined dynamically based on [budgetf]. Defaults to [7].

      - [~before]: Specifies an action to run on one domain before [init].

      - [~after]: Specifies an action to run on one domain after [work]. *)

  val to_thruput_metrics :
    n:int ->
    singular:string ->
    ?plural:string ->
    config:string ->
    ?unit_of_time:Unit_of_time.t ->
    ?unit_of_rate:Unit_of_rate.t ->
    t ->
    Metric.t list
  (** [to_thruput_metrics ~n ~singular ~config times] produces a pair of metrics
      from the recorded [times] where one metric is for the time a single
      operation takes and the other is the thruput of operations over all
      domains.

      Optional arguments:

      - [~plural]: Plural for the operation.  Defaults to [singular + "s"].

      - [~unit_of_time]: Unit of time for the duration of a single operation.
        Defaults to [`ns].

      - [~unit_of_rate]: Unit of rate for the number of operations per second.
        Defaults to [`M]. *)
end

module Suite : sig
  (** Dealing with benchmark suites. *)

  type t = budgetf:float -> Metric.t list
  (** Represents a benchmark suite, i.e. a function that produces a list of
      metric outputs for
      {{:https://github.com/ocurrent/current-bench}current-bench}. *)
end

module Cmd : sig
  (** Command line interface for a benchmark executable. *)

  val run :
    benchmarks:(string * Suite.t) list ->
    ?budgetf:float ->
    ?filters:string list ->
    ?debug:bool ->
    ?argv:string array ->
    ?flush:bool ->
    unit ->
    unit
  (** [run ~benchmarks ()] interprets command line arguments and runs the
      benchmarks suites based on the arguments.

      Optional arguments:

      - [~budgetf]: A budget (usually) in seconds passed to each benchmark
        suite.  This defaults to a small number so that a benchmark suite can be
        used as a test.

      - [~filters]: A list of regular expressions to match names of benchmark
        suites.  If any regular expression matches the name of benchmark, then
        that benchmark will be run.  Defaults to [[]].

      - [~argv]: Array of command line arguments.  Defaults to [Sys.argv].

      - [~flush]: Whether to flush the standard output after writing it.
        Defaults to [true].

      Command line arguments take precedence over the optional arguments.  In
      other words, you can specify the optional arguments to give defaults for
      the benchmark executable. *)
end

module Util : sig
  (** Utilities for creating benchmarks.

      ⚠️ In the future we expect to regroup these utilities under different
      modules and deprecate them in this module. *)

  val iter_factor : int
  (** A multiplier depending various factors such as whether we are running on a
      32- or 64-bit machine (1x/10x), bytecode or native (1x/10x), and whether
      we are running on single-core or multicore OCaml (1x/10x). *)

  val alloc : ?batch:int -> int Atomic.t -> int
  (** [alloc ~batch n] tries to decrement the specified atomic variable [n] by
      at most the optional amount [~batch] and not beyond [n] having value [0].
      Returns the amount by which [n] was decremented, which is [0] only in case
      [n] is [0]. *)

  val cross : 'a list -> 'b list -> ('a * 'b) list
  (** [cross xs ys] returns a list formed by pairing each element of [xs] with
      each element of [ys].

      For example:
      {[
        # Util.cross [1; 2; 3] ["a"; "b"]
        - : (int * string) list =
        [(1, "a"); (1, "b"); (2, "a"); (2, "b"); (3, "a"); (3, "b")]
      ]} *)

  module Bits : sig
    (** A minimalistic bitset data structure. *)

    type t
    (** Represents a bitset. *)

    val create : unit -> t
    (** [create ()] returns a new zero length bitset. *)

    val push : t -> bool -> unit
    (** [push bs b] adds the bit [b] to the end of the bitset [bs]. *)

    val iter : (bool -> unit) -> t -> unit
    (** [iter action bs] calls the [action] for each bit in the bitset [bs]. *)
  end

  val generate_push_and_pop_sequence : ?state:Random.State.t -> int -> Bits.t
  (** [generate_push_and_pop_sequence n] generates a bitset where each [true]
      bit represents a "push" operation and each [false] bit represents a
      "try_pop" operation.  Performing the operations on an initially empty
      dispenser leaves the dispenser empty.  The sequence may include "try_pop"
      operations at points where the dispenser will be empty. *)
end
