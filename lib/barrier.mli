type t

val make : int -> t
val await : t -> unit
val poison : t -> exn -> Printexc.raw_backtrace -> unit
