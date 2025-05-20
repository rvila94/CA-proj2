open Caml_compiler
open Cam_printer

type stack = value list

let rec eval_coms (code : coms) (stack : stack) : stack =
  match code with
  | [] -> stack
  | instr :: rest ->
    match instr, stack with
    | Quote v, _ :: stack' ->                         (* 4, 5, 6 *)
        eval_coms rest (v :: stack')

    | Car, Pair (v1, _) :: stack' ->                  (* 7 *)
        eval_coms rest (v1 :: stack')

    | Cdr, Pair (_, v2) :: stack' ->                  (* 8 *)
        eval_coms rest (v2 :: stack')

    | Cons, v1 :: v2 :: stack' ->                     (* 9 *)
        eval_coms rest (Pair (v2, v1) :: stack')

    | Push, v :: stack' ->                            (* 10 *)
        eval_coms rest (v :: v :: stack')

    | Swap, v1 :: v2 :: stack' ->                     (* 11 *)
        eval_coms rest (v2 :: v1 :: stack')

    | Op Add, Pair (Int n1, Int n2) :: stack' ->      (* 12 *)
        eval_coms rest (Int (n2 + n1) :: stack')

    | Op Sub, Pair (Int n1, Int n2) :: stack' ->      (* 12 *)
        eval_coms rest (Int (n2 - n1) :: stack')

    | Op Mult, Pair (Int n1, Int n2) :: stack' ->     (* 12 *)
        eval_coms rest (Int (n2 * n1) :: stack')

    | Op Eq, Pair (Int n1, Int n2) :: stack' ->       (* bonus *)
        eval_coms rest (Bool (n1 = n2) :: stack')

    | Branch (c1, _), Bool true :: stack' ->         (* 13 *)
        eval_coms (c1 @ rest) stack'

    | Branch (_, c2), Bool false :: stack' ->        (* 14 *)
        eval_coms (c2 @ rest) stack'

    | Cur c, v :: stack' ->                           (* 15 *)
        eval_coms rest (Closure (c, v) :: stack')

    | App, Pair (Closure (c, env), arg) :: stack' ->  (* 16 *)
        let new_stack = Pair(env, arg) :: stack' in
        eval_coms (c @ rest) new_stack

    | Rplac, v :: Closure (c, _) :: stack' ->         (* 17 *)
        let updated = Closure (c, v) in
        eval_coms rest (Pair (updated, v) :: stack')

    | instr, stack ->
        let msg = Printf.sprintf
          "Invalid instruction or stack.\nInstruction: %s\nStack: %s"
          (print_com instr)
          (String.concat " ; " (List.map print_value stack))
        in
        failwith msg


let eval (code : coms) : value =
  match eval_coms code [NullValue] with
  | v :: _ -> v
  | [] -> failwith "Empty stack at end of execution"
