open Caml_compiler


type stack = value list

let rec applatir (code : coms) =
  let code_applati = 
    match code with 
    |[] -> []
    | instr :: uction ->
      match instr with 
      | Branch (c1, c2) -> let padding = List.length c1 + 3 in (*3 = longueur d'un skip*)
                           let skip_padding = List.length c2 in
                           let retemp_code = Push :: Quote (Int skip_padding) :: Skip :: [] in
                           let temp_code = instr :: ((applatir c1) @ retemp_code @ (applatir c2) @ uction) in
                           (Push :: Quote (Int 0) :: Push :: Quote (Int padding) :: temp_code)
      | Cur c ->
        let offset = List.length (applatir uction) + 1 in
        let flat_c = applatir c in
        Push :: Quote (Int offset) :: instr :: (applatir uction @ flat_c) @ (Goto :: [])
      | _ -> instr :: (applatir uction)
      
  in
  code_applati


let dump_coms_to_ecl (filename : string) (code : coms) =
  let oc = open_out filename in
  let n = ((List.length code)+1) in
  Printf.fprintf oc "let bcode = create<%d>() ;;\n\n" n;
  Printf.fprintf oc "let load_code () =\n";

  List.iteri (fun k instr ->
    let line =
      match instr with
      | Quote v ->
          (match v with
           | Int n      -> Printf.sprintf "  set(bcode,%d,Quote_int %d);" k n
           | Bool b     -> Printf.sprintf "  set(bcode,%d,Quote_bool %b);" k b
           | NullValue  -> Printf.sprintf "  set(bcode,%d,Quotenull());" k
           | _ -> failwith "Unexpected value in Quote")

      | Push        -> Printf.sprintf "  set(bcode,%d,Push());" k
      | Car         -> Printf.sprintf "  set(bcode,%d,Car());" k
      | Cdr         -> Printf.sprintf "  set(bcode,%d,Cdr());" k
      | Cons        -> Printf.sprintf "  set(bcode,%d,Cons());" k
      | Swap        -> Printf.sprintf "  set(bcode,%d,Swap());" k
      | Rplac       -> Printf.sprintf "  set(bcode,%d,Rplac());" k
      | App         -> Printf.sprintf "  set(bcode,%d,App());" k

      | Op Add      -> Printf.sprintf "  set(bcode,%d,Add());" k
      | Op Sub      -> Printf.sprintf "  set(bcode,%d,Sub());" k
      | Op Mult     -> Printf.sprintf "  set(bcode,%d,Mult());" k

      (* cas non encore gérés proprement *)
      | Cur _       -> Printf.sprintf " set(bcode,%d,Closure());" k
      | Branch _    -> Printf.sprintf " set(bcode,%d,Branch());" k
      | Skip -> Printf.sprintf "  set(bcode,%d,Skip());" k
      | Goto -> Printf.sprintf "  set(bcode,%d,Goto());" k
    in
    Printf.fprintf oc "\n%s" line
  ) code;

  Printf.fprintf oc ";\n";
  close_out oc
