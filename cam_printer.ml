open Caml_compiler

(* Convertir un value en string *)
let rec print_value v =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | NullValue -> "null"
  | Pair (v1, v2) ->
      "(" ^ print_value v1 ^ ", " ^ print_value v2 ^ ")"
  | Closure (_, _) -> "<closure>"

(* Convertir une instruction CAM en string *)
let print_op = function
  | Add -> "add"
  | Sub -> "sub"
  | Mult -> "mult"

let print_com = function
  | Quote v -> "Quote(" ^ print_value v ^ ")"
  | Op op -> "Op(" ^ print_op op ^ ")"
  | Car -> "Car"
  | Cdr -> "Cdr"
  | Cons -> "Cons"
  | Push -> "Push"
  | Swap -> "Swap"
  | App -> "App"
  | Rplac -> "Rplac"
  | Cur _ -> "Cur(...)"
  | Branch (_, _) -> "Branch(...)"