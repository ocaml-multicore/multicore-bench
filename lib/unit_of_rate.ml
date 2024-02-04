type t = [ `_1 | `k | `M | `G ]

let to_divisor = function
  | `_1 -> 1.0
  | `k -> 1_000.0
  | `M -> 1_000_000.0
  | `G -> 1_000_000_000.0

let to_mnemonic = function
  | `_1 -> "1/s"
  | `k -> "k/s"
  | `M -> "M/s"
  | `G -> "G/s"
