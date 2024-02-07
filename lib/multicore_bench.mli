module Times : sig
  type t

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

  val average : t -> t
  val invert : t -> t
end

module Stats : sig
  type t

  val of_times : Times.t -> t
  val scale : float -> t -> t

  val to_json :
    name:string -> description:string -> units:string -> t -> Yojson.Safe.t list
end

module Suite : sig
  type t = budgetf:float -> Yojson.Safe.t list
end

module Cmd : sig
  val run : benchmarks:(string * Suite.t) list -> unit -> unit
end

module Rate : sig
  type t = [ `_1 | `k | `M | `G ]

  val to_divisor : t -> float
  val to_mnemonic : t -> string
end

module Unit_of_time : sig
  type t = [ `s | `ms | `mus | `ns ]

  val to_multiplier : t -> float
  val to_mnemonic : t -> string
end

module Util : sig
  val iter_factor : int
  val alloc : ?batch:int -> int Atomic.t -> int
  val cross : 'a list -> 'b list -> ('a * 'b) list

  val thruput_metrics :
    n:int ->
    singular:string ->
    ?plural:string ->
    config:string ->
    ?unit_of_time:Unit_of_time.t ->
    ?rate:Rate.t ->
    Times.t ->
    Yojson.Safe.t list

  module Bits : sig
    type t

    val create : unit -> t
    val push : t -> bool -> unit
    val iter : (bool -> unit) -> t -> unit
  end

  val generate_push_and_pop_sequence : ?state:Random.State.t -> int -> Bits.t
end
