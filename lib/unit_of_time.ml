type t = [ `s | `ms | `mus | `ns ]

let to_multiplier = function
  | `s -> 1.0
  | `ms -> 1_000.0
  | `mus -> 1_000_000.0
  | `ns -> 1_000_000_000.0

let to_mnemonic = function
  | `s -> "s"
  | `ms -> "ms"
  | `mus -> "μs"
  | `ns -> "ns"
