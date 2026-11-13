type t = C | Cpp | Rust | OCaml | Python

let of_string = function "c" -> C | "cpp" -> Cpp | "rs" -> Rust | "ml" -> OCaml | "py" -> Python | x -> failwith x
let to_string = function C -> "c" | Cpp -> "cpp" | Rust -> "rs" | OCaml -> "ml" | Python -> "py"
