open Caml_compiler
open Cam_interpreter
open Cam_printer

(* Liste de tests MiniML → CAM → Résultat *)
let tests : (string * expr * string) list = [
  ("test1", Number 1, "Int 1");
  ("test2", True, "Bool true");
  ("test3", If (True, Number 1, Number 2), "Int 1");
  ("test4", If (False, Number 1, Number 2), "Int 2");
  ("test5", Mlpair (Number 1, Number 2), "(Int 1, Int 2)");
  ("test6", Apply (Ident "add", Mlpair (Number 1, Number 2)), "Int 3");
]

(* exécuter une expression MiniML complète *)
let run (e : expr) : value =
  let cam_code = compile e [] in
  Printf.printf "\nCAM code: %s\n"
    (String.concat " ; " (List.map print_com cam_code));
  eval cam_code

let () =
  List.iter (fun (name, expr, expected) ->
    let result = run expr in
    Printf.printf "%s:\n" name;
    Printf.printf "  Result: %s\n" (print_value result);
    Printf.printf "  Expected: %s\n\n" expected
  ) tests
