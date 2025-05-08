open Caml_compiler
open Cam_interpreter

(* exécuter une expression MiniML complète *)
let run (e : expr) : value =
  let cam_code = compile e [] in
  eval cam_code

(* Convertir un value en string *)
let rec string_of_value v =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | NullValue -> "null"
  | Pair (v1, v2) ->
      "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
  | Closure (_, _) -> "<closure>"

(* Convertir une instruction CAM en string *)
let string_of_op = function
  | Add -> "add"
  | Sub -> "sub"
  | Mult -> "mult"

let rec string_of_com = function
  | Quote v -> "Quote(" ^ string_of_value v ^ ")"
  | Op op -> "Op(" ^ string_of_op op ^ ")"
  | Car -> "Car"
  | Cdr -> "Cdr"
  | Cons -> "Cons"
  | Push -> "Push"
  | Swap -> "Swap"
  | App -> "App"
  | Rplac -> "Rplac"
  | Cur _ -> "Cur(...)"
  | Branch (_, _) -> "Branch(...)"

(* Liste de tests MiniML → CAM → Résultat *)
let tests : (string * expr * string) list = [
  ("test1", Number 1, "Int 1");
  ("test2", True, "Bool true");
  ("test3", If (True, Number 1, Number 2), "Int 1");
  ("test4", Mlpair (Number 1, Number 2), "(Int 1, Int 2)");
  ("test5", Apply (Ident "add", Mlpair (Number 1, Number 2)), "Int 3");
]

let () =
  List.iter (fun (name, expr, expected) ->
    let result = run expr in
    Printf.printf "%s:\n" name;
    Printf.printf "  Result: %s\n" (string_of_value result);
    Printf.printf "  Expected: %s\n\n" expected
  ) tests
