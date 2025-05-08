open Caml_compiler

type stack = value list

let rec eval_coms (code : coms) (stack : stack) : stack =
  match code with
  | [] -> stack
  | instr :: rest ->
    match instr, stack with
    | Quote v, _ ->                                   (* 4?, 5, 6 *) (* à verif pour le 4 *)
        eval_coms rest (v :: stack)

    | Car, Pair (v1, _) :: stack' ->                  (* 7 *)
        eval_coms rest (v1 :: stack')

    | Cdr, Pair (_, v2) :: stack' ->                  (* 8 *)
        eval_coms rest (v2 :: stack')

    | Cons, v1 :: v2 :: stack' ->                     (* 9 *)
        eval_coms rest (Pair (v2, v1) :: stack')

    | Push, s ->                                      (* 10 *) (* à verif *)
        eval_coms rest (NullValue :: s)

    | Swap, v1 :: v2 :: stack' ->                     (* 11 *)
        eval_coms rest (v2 :: v1 :: stack')

    | Op Add, Int n1 :: Int n2 :: stack' ->           (* 12 *)
        eval_coms rest (Int (n2 + n1) :: stack')

    | Op Sub, Int n1 :: Int n2 :: stack' ->           (* 12 *)
        eval_coms rest (Int (n2 - n1) :: stack')

    | Op Mult, Int n1 :: Int n2 :: stack' ->          (* 12 *)
        eval_coms rest (Int (n2 * n1) :: stack')

    | Branch (c1, c2), Bool true :: stack' ->         (* 13 *)
        eval_coms (c1 @ rest) stack'

    | Branch (c1, c2), Bool false :: stack' ->        (* 14 *)
        eval_coms (c2 @ rest) stack'

    | Cur c, v :: stack' ->                           (* 15 *)
        eval_coms rest (Closure (c, v) :: stack')

    | App, Pair (Closure (c, env), arg) :: stack' ->  (* 16 *) (* à verif *)
        let new_stack = env :: arg :: stack' in
        eval_coms (c @ rest) new_stack

    | Rplac, v :: Closure (c, _) :: stack' ->         (* 17 *) (* à verif *)
        eval_coms rest (Closure (c, v) :: stack')

    | _ ->
        failwith "Invalid"
