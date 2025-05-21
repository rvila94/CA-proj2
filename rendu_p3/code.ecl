type const = 
  Integer of int
  |Boolean of bool
  |Pair_ii of int * int
  |Pair_bb of bool * bool
  |Pair_ib of int * bool
  |Pair_bi of bool * int
  |Null of unit
;;

type instruction = 
  Quote_bool of bool
  |Quote_int of int
  |Push of unit
  |EOF of unit
  |Add of unit
  |Sub of unit
  |Mult of unit
  |Cons of unit
  |Car of unit
  |Cdr of unit
  |Swap of unit
  |Skip of unit
  |Branch of unit
  |Closure of unit
  |Goto of unit
;;

let bcode = create<256>();;
let stack = create<256>();;
let closure_stack = create<256>();;

let print_const (c : const) =
  match c with
  | Integer x ->
      print_int x
  | Boolean b ->
      print_string "bool"
  | Pair_ii (x, y) ->
      print_int x; print_string ", "; print_int x
  | Pair_bb (b1, b2) ->
      print_string "pair bb"
  | Pair_ib (x, b) ->
      print_string "pair"
  | Pair_bi (b, x) ->
      print_string "pair"
  | Null () ->
      print_string "null"
;;



let run_interp() = 
    let rec vm(pc, sp) = 
      let instr = get(bcode, pc) in
      match instr with
        | Quote_int x -> 
          set(stack, sp-1, Integer x);
          vm(pc+1, sp) 
        | Quote_bool b -> 
          set(stack, sp-1, Boolean b);
          vm(pc+1, sp)
        | Push() ->
            let () = 
              if sp=0 then
                let () = print_string "pushing in void" in
                print_newline()
              else 
                let () = print_string "pushing"; print_newline() in
                let temp = get(stack, sp-1) in
                set(stack, sp, temp);
                print_newline()
            in
            vm(pc+1, sp+1)
        | Swap() ->
            print_string "swapping"; print_newline();
            let temp_1 = get(stack, sp-1) in
            let temp_2 = get(stack, sp-2) in
            set(stack, sp-2, temp_1);
            set(stack, sp-1, temp_2);
            vm(pc+1, sp)
        | Cons() ->
            print_string "pairing"; print_newline();
            let temp_1 = get(stack, sp-1) in
            let temp_2 = get(stack, sp-2) in
            let () = match temp_1 with
              |Integer x -> 
                let () = match temp_2 with
                |Integer y -> let temp_3 = Pair_ii (x, y) in set(stack, sp-2, temp_3)
                |Boolean b -> let temp_3 = Pair_ib (x, b) in set(stack, sp-2, temp_3)
                |_ -> print_newline()
                in
                print_newline()
              |Boolean b -> 
                let () = match temp_2 with
                |Integer y -> let temp_3 = Pair_bi (b, y) in set(stack, sp-2, temp_3)
                |Boolean t -> let temp_3 = Pair_bb (b, t) in set(stack, sp-2, temp_3)
                |_ -> print_newline()
                in
                print_newline()
              |_ -> print_newline ()
            in
            vm(pc+1, sp-1)
        | Car() -> 
            print_string "car"; print_newline();
            let temp = get(stack, sp-1) in
            let () = match temp with
              | Pair_bb (b, t) -> set(stack, sp-1, Boolean b)
              | Pair_bi (b, x) -> set(stack, sp-1, Boolean b)
              | Pair_ib (x, b) -> set(stack, sp-1, Integer x)
              | Pair_ii (x, y) -> set(stack, sp-1, Integer x)
              | _ -> print_string "supposed to be pair"
            in
            vm(pc+1, sp)
        | Cdr() -> 
          print_string "car"; print_newline();
          let temp = get(stack, sp-1) in
          let () = match temp with
            | Pair_bb (b, t) -> set(stack, sp-1, Boolean t)
            | Pair_bi (b, x) -> set(stack, sp-1, Integer x)
            | Pair_ib (x, b) -> set(stack, sp-1, Boolean b)
            | Pair_ii (x, y) -> set(stack, sp-1, Integer y)
            | _ -> print_string "supposed to be pair"
          in
          vm(pc+1, sp)
         | Add() ->
            print_string "adding"; print_newline();
            let temp_1 = get(stack, sp-1) in
            let temp_2 = get(stack, sp-2) in 
            let () = match temp_1 with
            |Integer x ->
              let () = match temp_2 with
              |Integer y -> let temp_res = Integer (x + y) in 
                            set(stack, sp-2, temp_res)
              | _ -> print_string "suposed to be int"
              in
              print_newline ()
            | _ -> print_string "suposed to be int"
            in
            vm(pc+1, sp-1)
        | Sub() ->
          print_string "subbing"; print_newline();
            let temp_1 = get(stack, sp-1) in
            let temp_2 = get(stack, sp-2) in 
            let () = match temp_1 with
            |Integer x ->
              let () = match temp_2 with
              |Integer y -> let temp_res = Integer (x - y) in 
                            set(stack, sp-2, temp_res)
              | _ -> print_string "suposed to be int"
              in
              print_newline ()
            | _ -> print_string "suposed to be int"
            in
            vm(pc+1, sp-1)
        | Mult() ->
          print_string "multing"; print_newline();
            let temp_1 = get(stack, sp-1) in
            let temp_2 = get(stack, sp-2) in 
            let () = match temp_1 with
            |Integer x ->
              let () = match temp_2 with
              |Integer y -> let temp_res = Integer (x * y) in 
                            set(stack, sp-2, temp_res)
              | _ -> print_string "suposed to be int"
              in
              print_newline ()
            | _ -> print_string "suposed to be int"
            in
            vm(pc+1, sp-1)
        | Branch() ->
            let temp_pad = get(stack, sp-1) in 
            let temp_bool = get(stack, sp-3) in
            let padding = 
              match temp_bool with
              |Boolean b -> 
                let temp = match temp_pad with
                  |Integer i -> if b then 1 else i+1
                  | _ -> let () = print_string "supposed to be integer" in 1
                in
                temp
              | _ -> let () = print_string "supposed to be boolean" in 1
            in
            print_string "padding : "; print_int padding; print_newline();
            vm(pc+padding, sp-2)
        | Skip() ->
          print_string "skipping :";
          let temp_pad = get(stack, sp-1) in 
          let pad = 
            match temp_pad with
            |Integer i -> i+1
            |_ -> 1
          in
          print_int pad; print_newline();
          vm(pc+pad, sp-1)
        | EOF() ->
            print_string "End of file, top of stack : "; print_newline();
            let res = get(stack, sp-1) in
            print_const res; print_newline()
        | _ ->
            print_string "End of file, top of stack : "; print_newline();
            let res = get(stack, sp-1) in
            print_const res; print_newline()
    in
    set(stack, 0, Null());
    vm(0, 1)
  ;;