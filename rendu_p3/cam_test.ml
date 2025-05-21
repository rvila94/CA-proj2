open Caml_compiler
open Cam_applatisseur

(* Liste de tests MiniML → CAM → Résultat *)
let tests : (string * expr * string) list = [
  ("test1", Number 1, "Int 1");
  ("test2", True, "Bool true");
  ("test3", If (True, Number 1, Number 2), "Int 1");
  ("test4", If (False, Number 1, Number 2), "Int 2");
  ("test5", Mlpair (Number 1, Number 2), "(Int 1, Int 2)");
  ("test6", Apply (Ident "add", Mlpair (Number 1, Number 2)), "Int 3");
  ("test10", Apply (Lambda (IdentPat "x", Number 1), Number 2), "1");
  ("test11", Apply (Lambda (IdentPat "x", Ident "x"), Number 1), "1");
]

(* exécuter une expression MiniML complète *)
let run (e : expr) (name : string) : unit =
  let cam_code = compile e [] in
  let filename = name ^ ".ecl" in
  let code_applati = applatir cam_code in 
  dump_coms_to_ecl filename code_applati;
  Printf.printf "%s → %s generated.\n" name filename

let () =
  List.iter (fun (name, expr, _expected) ->
    run expr name
  ) tests

