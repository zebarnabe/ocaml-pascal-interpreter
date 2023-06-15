(*
  devolve o valor de uma variavel na memoria

  returns the value of a variable in memory  
*)
let rec get_val_from_mem id memory =
match memory with
| [] -> if (id="true") then (Const_bool true) else (if (id="false") then (Const_bool false) else raise( Failure "Internal Error"))
| {Nome = j; Value = tp}::t -> if j=id then tp else get_val_from_mem id t;;

(*
  define o valor de uma variavel na memoria

  Defines the value of a variable in memory
*)
let rec set_val_from_mem id memory val =
match memory with
| [] -> []
| {Nome = j; Value = tp}::t -> if j=id then {Nome = j; Value = val}::t else {Nome = j; Value = tp}::(set_val_from_mem id t val);;

(*
  inverte uma lista
  
  inverts a list
*)
let rec invert_list lista =
match lista with
| [] -> []
| h::t -> (invert_list t)@[h];;

(*
  devolve o numero de elementos duma lista

  returns the number of elements in a list
*)
let rec lenght_list lista =
match lista with
| [] -> 0
| h::t -> 1+(lenght_list t);;

(*
  potencia de inteiros

  integer exponentiation
*)
let rec pot n1 n2 = n1*(pot n1 (n2-1));;

(*
  resolve expressoes aritmetica em RPN invertido

  solves arithmetic expressions in inverted RPN  
*)
let solve_arithm expr memory =
let rec rpn_solve stack expr memory =
match expr with
| [] -> (match stack with
  | [NUMBER n] -> NUMBER n
  | [IDENT "true"] -> IDENT "true"
  | [IDENT "false"] -> IDENT "false"
  | [IDENT a] -> IDENT a
  | _ -> raise( Failure "Intenal Error")
  )
| (NUMBER nm)::t -> rpn_solve ((NUMBER nm)::stack) t memory 
| (IDENT id)::t -> (match (get_val_from_mem id memory) with
  | Var_int n -> rpn_solve ((NUMBER n)::stack) t memory
  | Const_int n -> rpn_solve ((NUMBER n)::stack) t memory
  | Var_bool n -> rpn_solve ((IDENT id)::stack) t memory
  | Const_bool n -> rpn_solve ((IDENT id)::stack) t memory
  | _ -> raise( Failure "Internal Error")
  )
| ((PLUS|MINUS|STARSTAR|STAR|DIV|MOD|EQUAL|NOTEQUAL|LE|LT|GE|GT) as tok)::t -> (match stack with
  | tok1::tok2::s -> let stack = s in
    let val1 = (match tok1 with
    | NUMBER n -> n 
    | IDENT id -> (match (get_val_from_mem id memory) with
      | Var_int n -> n
      | Const_int n -> n
      | _ -> raise( Failure "Internal Error")
      )
    | _ -> raise( Failure "Internal Error")
    ) in
    let val2 = (match tok2 with
    | NUMBER n -> n 
    | IDENT id -> (match (get_val_from_mem id memory) with
      | Var_int n -> n
      | Const_int n -> n
      | _ -> raise( Failure "Internal Error")
      )
    | _ -> raise( Failure "Internal Error")
    ) in 
    (match tok with
    | PLUS -> rpn_solve ((NUMBER (val1+val2))::stack) t memory 
    | MINUS -> rpn_solve ((NUMBER (val1-val2))::stack) t memory 
    | STARSTAR -> rpn_solve ((NUMBER (pot val1 val2))::stack) t memory 
    | STAR -> rpn_solve ((NUMBER (val1 * val2))::stack) t memory 
    | DIV -> rpn_solve ((NUMBER (val1 / val2))::stack) t memory 
    | MOD -> rpn_solve ((NUMBER (val1 mod val2))::stack) t memory 
    | EQUAL -> if (val1 = val2) then (rpn_solve ((IDENT "true")::stack) t memory)
             else (rpn_solve ((IDENT "false")::stack) t memory)
    | NOTEQUAL -> if (val1 = val2) then (rpn_solve ((IDENT "false")::stack) t memory)
             else (rpn_solve ((IDENT "true")::stack) t memory)
    | LE -> if (val1 <= val2) then (rpn_solve ((IDENT "true")::stack) t memory)
             else (rpn_solve ((IDENT "false")::stack) t memory) 
    | LT -> if (val1 < val2) then (rpn_solve ((IDENT "true")::stack) t memory)
             else (rpn_solve ((IDENT "false")::stack) t memory)
    | GE -> if (val1 >= val2) then (rpn_solve ((IDENT "true")::stack) t memory)
             else (rpn_solve ((IDENT "false")::stack) t memory)
    | GT -> if (val1 > val2) then (rpn_solve ((IDENT "true")::stack) t memory)
             else (rpn_solve ((IDENT "false")::stack) t memory)
    | _ -> raise( Failure "Internal Error")
    )
  | _ -> raise( Failure "Internal Error")
  )
| ((AND|OR) as tok)::t -> (match stack with
  | tok1::tok2::s -> let stack = s in
    let val1 = (match tok1 with
    | IDENT id -> (match (get_val_from_mem id memory) with
      | Var_bool n -> n
      | Const_bool n -> n
      | _ -> raise( Failure "Internal Error")
      )
    | _ -> raise( Failure "Internal Error")
    ) in
    let val2 = (match tok2 with
    | IDENT id -> (match (get_val_from_mem id memory) with
      | Var_bool n -> n
      | Const_bool n -> n
      | _ -> raise( Failure "Internal Error")
      )
    | _ -> raise( Failure "Internal Error")
    ) in 
    (match tok with
    | AND -> if (val1 && val2) then (rpn_solve ((IDENT "true")::stack) t memory)
             else (rpn_solve ((IDENT "false")::stack) t memory)
    | OR -> if (val1 || val2) then (rpn_solve ((IDENT "true")::stack) t memory)
            else (rpn_solve ((IDENT "false")::stack) t memory)
    | _ -> raise( Failure "Internal Error")
    )
  | _ -> raise( Failure "Internal Error")
  )
| (NOT)::t -> (match stack with
  | tok1::s -> let stack = s in
    let val1 = (match tok1 with
    | IDENT id -> (match (get_val_from_mem id memory) with
      | Var_bool n -> n
      | Const_bool n -> n
      | _ -> raise( Failure "Internal Error")
      )
    | _ -> raise( Failure "Internal Error")
    ) in
    ( if (val1) then (rpn_solve ((IDENT "false")::stack) t memory)
      else (rpn_solve ((IDENT "true")::stack) t memory)
    )
  | _ -> raise( Failure "Internal Error")
  )
| _ -> raise( Failure "Internal Error") in

let expr = invert_list expr in
let final = rpn_solve [] expr memory in
match final with
  | NUMBER n -> Var_int n
  | IDENT "true" -> Var_bool true
  | IDENT "false" -> Var_bool false
  | IDENT a -> (get_val_from_mem a memory)
  | _ -> raise( Failure "Intenal Error")
;;

(*
  Executa o programa, utiliza a funcao interna exec_while para cuidar de todas as instrucoes ciclicas

  Runs the program, uses the internal function exec_while to take care of all the cyclic instructions  
*)
let rec run_program rip =
let rec exec_while expr instr memory =
  let go = (match (solve_arithm expr memory) with
    | Var_bool g -> g
    | _ -> raise( Failure "Internal Error")
    ) in
      if (go) then (
        let memory = (run_program (memory,instr)) in (exec_while expr instr memory)
        )
      else memory
in
let memory = (first rip) in
let instr = (second rip) in
match instr with
| Vazia -> memory
| No ([],[]) -> memory
| No ([ASSIGNMENT],args) -> (match args with
  | No ([IDENT id],[])::No(expr,[])::rest -> let memory = (set_val_from_mem id memory (solve_arithm expr memory) ) in
    (match rest with
    | [] -> memory
    | [rest] -> run_program (memory,rest)
    | _ -> raise( Failure "Internal Error")
    )
  | _ -> raise( Failure "Internal Error")
  )
| No ([IF],args) -> (match args with
  | No (expr,[])::ifthen::ifelse::rest -> let go = (match (solve_arithm expr memory) with
    | Var_bool g -> g
    | _ -> raise( Failure "Internal Error")
    ) in
    let memory = (if go then run_program (memory,ifthen) else run_program (memory,ifelse)) in
    (match rest with
    | [] -> memory
    | [rest] -> run_program (memory,rest)
    | _ -> raise( Failure "Internal Error")
    )
  | _ -> raise( Failure "Internal Error")
  )
| No ([FOR],args) -> (match args with
  | No ([IDENT id],[])::No (expr1,[])::No (expr2,[])::instr::rest -> let memory = run_program (memory,
             No ([ASSIGNMENT],[No ([IDENT id],[]); No (MINUS::NUMBER 1::expr1,[]); No ([WHILE],[ No ((LT)::(IDENT id)::expr2,[]); No ([BEGIN],[ No ([ASSIGNMENT],[ No ([IDENT id],[]); No ([PLUS;IDENT id;NUMBER 1],[]); instr])])])])) in
    (match rest with
    | [] -> memory
    | [rest] -> run_program (memory,rest)
    | _ -> raise( Failure "Internal Error")
    ) 
  | _ -> raise( Failure "Internal Error")
  )
| No ([REPEAT],args) -> (match args with
  | No (expr,[])::instr::rest -> let memory = run_program (memory,instr) in
    let memory = (exec_while (NOT::expr) instr memory) in
    (match rest with
    | [] -> memory
    | [rest] -> run_program (memory,rest)
    | _ -> raise( Failure "Internal Error")
    ) 
  | _ -> raise( Failure "Internal Error")
  )
| No ([WHILE],args) -> (match args with
  | No (expr,[])::instr::rest -> let memory = (exec_while expr instr memory) in
    (match rest with
    | [] -> memory
    | [rest] -> run_program (memory,rest)
    | _ -> raise( Failure "Internal Error")
    ) 
  | _ -> raise( Failure "Internal Error")
  )
| No ([BEGIN],args) -> (match args with
  | instr::rest -> let memory = run_program (memory,instr) in    
    (match rest with
    | [] -> memory
    | [rest] -> run_program (memory,rest)
    | _ -> raise( Failure "Internal Error")
    ) 
  | _ -> raise( Failure "Internal Error")
  )
| No ([WRITELN],args) -> (match args with
  | [] -> raise( Failure "Internal Error")
  | [rest] -> (print_string "\n"; run_program (memory,rest))
  | No ([STRING show],[])::rest -> (print_string show; run_program (memory,No ([WRITELN],rest)))
  | No (expr,[])::rest -> let val = (solve_arithm expr memory) in 
    (match val with
    | (Var_int n) -> (print_string (string_of_int n); run_program (memory,No ([WRITELN],rest)))
    | (Var_bool true) -> (print_string "TRUE"; run_program (memory,No ([WRITELN],rest)))
    | (Var_bool false) -> (print_string "FALSE"; run_program (memory,No ([WRITELN],rest)))
    | _ -> raise( Failure "Internal Error")
    )
  | _ -> raise( Failure "Internal Error") 
  )
| No ([WRITE],args) -> (match args with
  | [] -> raise( Failure "Internal Error")
  | [rest] -> (run_program (memory,rest))
  | No ([STRING show],[])::rest -> (print_string show; run_program (memory,No ([WRITE],rest)))
  | No (expr,[])::rest -> let val = (solve_arithm expr memory) in 
    (match val with
    | (Var_int n) -> (print_string (string_of_int n); run_program (memory,No ([WRITE],rest)))
    | (Var_bool true) -> (print_string "TRUE"; run_program (memory,No ([WRITE],rest)))
    | (Var_bool false) -> (print_string "FALSE"; run_program (memory,No ([WRITE],rest)))
    | _ -> raise( Failure "Internal Error")
    )
  | _ -> raise( Failure "Internal Error") 
  )
| _ -> raise( Failure "Internal Error")
;;


