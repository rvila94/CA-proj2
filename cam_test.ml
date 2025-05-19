open Caml_compiler
open Cam_interpreter
open Cam_printer

(* Liste de tests MiniML → CAM → Résultat *)
let tests : (string * expr * string) list = [
  ("test1", Number 1, "1");
  ("test2", True, "true");
  ("test3", If (True, Number 1, Number 2), "1");
  ("test4", If (False, Number 1, Number 2), "2");
  ("test5", Mlpair (Number 1, Number 2), "(1, 2)");
  ("test6", Apply (Ident "add", Mlpair (Number 1, Number 2)), "3");
  ("test7", Let (IdentPat "x", Number 1, Number 2), "2");
  (* à partir d'ici les test ne passent pas (sauf 10) + faudra ajouter d'autres tests *)
  ("test8", Let (IdentPat "x", Number 1, Ident "x"), "1");
  ("test9", Let (IdentPat "x", Number 1,
             Let (IdentPat "y", Number 2, Ident "x")), "1");
  ("test10", Apply (Lambda (IdentPat "x", Number 1), Number 2), "1");
  ("test11", Apply (Lambda (IdentPat "x", Ident "x"), Number 1), "1");
  ("test12", Let (IdentPat "x",
              Let (IdentPat "y", Number 1, Ident "y"),
              Ident "x"), "1");
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
