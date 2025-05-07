(* from a 1986 paper: https://www.cs.tufts.edu/~nr/cs257/archive/dominique-clement/applicative.pdf *)

(* figure 1 page 14 (Abstract Syntax of Mini-ML) *)
type ident = string

type pat =
| Pairpat of pat * pat
| IdentPat of ident
| NullPat

type expr =
| Ident of ident
| Number of int
| False
| True
| Apply of expr * expr
| Mlpair of expr * expr
| Lambda of pat * expr
| Let of pat * expr * expr
| LetRec of pat * expr * expr
| If of expr * expr * expr


(* figure 7 page 21 (Abstract syntax of CAM code) *)
type program = coms
and coms = com list
and com =
| Quote of value
| Op of operator
| Car
| Cdr
| Cons
| Push
| Swap
| App
| Rplac
| Cur of coms
| Branch of coms * coms
and value =
| Int of int
| Bool of bool
| NullValue

and operator = Add | Sub | Mult

(* Figure 10 page 24 (Translation from Mini-ML to CAM) *)
let rec compile (e: expr) (env : (ident * value) list) : coms =
  match e with
  | Number n -> [Quote (Int n)]                          (* 2 *)
  | True -> [Quote (Bool true)]                          (* 3 *)
  | False -> [Quote (Bool false)]                        (* 4 *)
  | Ident i ->                                           (* 5 *) (* à verif *)
      let value = List.assoc i env in 
      [Quote(value)]

  | If (e1, e2, e3) ->                                   (* 6 *)
      let c1 = compile e1 env in
      let c2 = compile e2 env in
      let c3 = compile e3 env in
      Push :: c1 @ [Branch (c2, c3)]

  | Mlpair (e1, e2) ->                                   (* 7 *)
      let c1 = compile e1 env in
      let c2 = compile e2 env in
      [Push] @ c1 @ [Swap] @ c2 @ [Cons]

  | Let (p, e1, e2) ->                                   (* 8 *)
      let c1 = compile e1 env in
      let new_env = match p with
        | IdentPat(x) -> (x, NullValue) :: env
        | _ -> env 
      in
      let c2 = compile e2 new_env in
      Push :: c1 @ [Cons] @ c2

  | LetRec (p, e1, e2) ->                                (* 9 *)
      let new_env = match p with
        | IdentPat(x) -> (x, NullValue) :: env
        | _ -> env
      in
      let c1 = compile e1 new_env in
      let c2 = compile e2 new_env in
      [Push; Quote(NullValue); Cons; Push] @ c1 @ [Swap; Rplac] @ c2

  | Lambda (p, e) ->                                    (* 10 *)
      let new_env = match p with
        | IdentPat(x) -> (x, NullValue) :: env
        | _ -> env 
      in
      let c = compile e new_env in
      [Cur(c)]

  | Apply (e1, e2) ->                                   (* 12 *)
      match e1 with
      | Ident("add") -> compile e2 env @[Op(Add)]
      | Ident("sub") -> compile e2 env @[Op(Sub)]
      | Ident("mult") -> compile e2 env @[Op(Mult)]
      | _ -> 
          let c1= compile e1 env in 
          let c2= compile e2 env in 
          Push :: c1 @ [Swap] @ c2 @ [Cons;App]

  