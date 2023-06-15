(*
  Uma estrutura para a arvore sintatica: tokens list tree (arvore) *)
type 'a arvore = Vazia | No of 'a*'a arvore list;;

(*
  Todos os tipos englobados no interpretador: variaveis, constantes, inteiros, booleanos e nao definidos 
  All types recognized by the interpreter: variables, constants, integers, booleans and undefined
*)
type tipo = Var_int of int | Const_int of int | Var_bool of bool | Const_bool of bool
	    | Var_int_nd | Var_bool_nd;;

(*
  A alocaçao de memoria para programa
  Memory allocation  
*)

type allocation = {Nome : string; Value : tipo};;

(*
A RIP é definida internamente do seguinte modo

RIP:
(alocation list, tokens list arvore)
*)


(*
  Converte uma stream para uma lista, até que encontre um elemento de paragem
  Converts a strems into a list, until a stop element is found  
*)
let rec list_of_stream stop list i =
match i with
| [< 'a; i >] -> if (a=stop) then list else list_of_stream stop (list@[[a]]) i
| [< >] -> raise( Failure "Syntax Error - Unexpected end of stream");;

(*
  Obtém o valor de uma variavel com determinado nome
  Gets the value of the variable with a given name
*)
let rec get_type i memory =
match memory with
| [] -> if (i="true") then (Const_bool true) else (if (i="false") then (Const_bool false) else raise( Failure "Syntax Error - Variable not declared"))
| {Nome = j; Value = tp}::t -> if j=i then tp else get_type i t;;

(*
  Define a variavel inicializando-a a zero ou true
  Defines a variable, initializes it at zero/true
*)
let rec put_value i memory =
match memory with
| [] -> []
| {Nome = j; Value = tp}::t ->   let val = (match tp with
  | (Var_int it) -> (Var_int it)
  | (Const_int it) -> (Const_int it)
  | (Var_bool bl) -> (Var_bool bl)
  | (Const_bool bl) -> (Const_bool bl)
  | (Var_int_nd) -> (Var_int 0)
  | (Var_bool_nd) -> (Var_bool true)
  ) in if i=j then {Nome = j; Value = val}::t else {Nome = j; Value = tp}::(put_value i t);;

(*
 As seguintes funçoes sao utilizadas para criar expressoes RPN invertidas
 The following functions are used to create expressions in an inverted RPN form
*)

(* 
  Encontra os NOT's e converte a expressao para RPN (invertida)
  Solve for NOTs
*)  
let rec solve_not expr memory =
match expr with
| [] -> []
| [NOT as tok]::[NUMBER n]::t -> raise( Failure "Syntax Error - operator not can only be applied to booleans")
| [NOT as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> raise( Failure "Syntax Error - operator not can only be applied to booleans")
  | (Const_int it) -> raise( Failure "Syntax Error - operator not can only be applied to booleans")
  | (Var_bool bl) -> [tok;IDENT i]::(solve_not t memory)
  | (Const_bool bl) -> [tok;IDENT i]::(solve_not t memory)
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [NOT]::[NOT]::t -> (solve_not t memory)
| [NOT as tok]::(x::y)::t -> (match x with
  | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> (tok::x::y)::(solve_not t memory)
  | _ -> raise( Failure "Syntax Error - operator not can only be applied to booleans")
  )
| h::t -> h::(solve_not t memory);;

(*
  Encontra os STARSTAR's e converte a expressao para RPN (invertida)
  Solve for **
*)  
let rec solve_starstar expr memory =
match expr with
| [] -> []
| [NUMBER n]::[STARSTAR as tok]::[NUMBER m]::t -> (solve_starstar ([tok;NUMBER n;NUMBER m]::t) memory)
| [NUMBER n]::[STARSTAR as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> (solve_starstar ([tok;NUMBER n;IDENT i]::t) memory)
  | (Const_int it) -> (solve_starstar ([tok;NUMBER n;IDENT i]::t) memory)
  | (Var_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  | (Const_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [IDENT i]::[STARSTAR as tok]::[NUMBER n]::t -> (match (get_type i memory) with
  | (Var_int it) -> (solve_starstar ([tok;IDENT i;NUMBER n]::t) memory)
  | (Const_int it) -> (solve_starstar ([tok;IDENT i;NUMBER n]::t) memory)
  | (Var_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  | (Const_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [IDENT i]::[STARSTAR as tok]::[IDENT j]::t -> (match (get_type i memory) with
  | (Var_int it) -> (match (get_type j memory) with
    | (Var_int it) -> (solve_starstar ([tok;IDENT i;IDENT j]::t) memory)
    | (Const_int it) -> (solve_starstar ([tok;IDENT i;IDENT j]::t) memory)
    | (Var_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
    | (Const_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
  | (Const_int it) -> (match (get_type i memory) with
    | (Var_int it) -> (solve_starstar ([tok;IDENT i;IDENT j]::t) memory)
    | (Const_int it) -> (solve_starstar ([tok;IDENT i;IDENT j]::t) memory)
    | (Var_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
    | (Const_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
  | (Var_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  | (Const_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [NUMBER n]::[STARSTAR as tok]::(x::y)::t -> (match x with
  | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (solve_starstar ((tok::NUMBER n::x::y)::t) memory)
  | _ -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  )
| (x::y)::[STARSTAR as tok]::[NUMBER n]::t -> (match x with
  | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (solve_starstar (((tok::x::y)@[NUMBER n])::t) memory)
  | _ -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  )
| [IDENT i]::[STARSTAR as tok]::(x::y)::t -> (match (get_type i memory) with
  | (Var_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (solve_starstar ((tok::IDENT i::x::y)::t) memory)
    | _ -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
    )
  | (Const_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (solve_starstar ((tok::IDENT i::x::y)::t) memory)
    | _ -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
    )
  | (Var_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  | (Const_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[STARSTAR as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (solve_starstar (((tok::x::y)@[IDENT i])::t) memory)
    | _ -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
    )
  | (Const_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (solve_starstar (((tok::x::y)@[IDENT i])::t) memory)
    | _ -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
    )
  | (Var_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  | (Const_bool bl) -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[STARSTAR as tok]::(k::l)::t -> (match x with
  | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (match k with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (solve_starstar (((tok::x::y)@(k::l))::t) memory)
    | _ -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
    )
  | _ -> raise( Failure "Syntax Error - operator ** can only be applied to integers")
  )
| h::t -> h::(solve_starstar t memory);;

(*
  Encontra (detecta) os operadores unarios (+/-) e converte a expressao para RPN (invertida)
  Solve for unary + | -
*)  
let rec solve_unary expr memory =
  let rec solve_unary_rest expr memory =
  match expr with
  | [] -> []
  | [(PLUS|MINUS|DIV|MOD|STAR|STARSTAR) as op]::[MINUS]::[NUMBER n]::t -> [op]::[NUMBER (-n)]::solve_unary_rest t memory         
  | [(PLUS|MINUS|DIV|MOD|STAR|STARSTAR) as op]::[MINUS]::[IDENT i]::t -> (match (get_type i memory) with
    | (Var_int it) -> [op]::[STAR;IDENT i;NUMBER (-1)]::solve_unary_rest t memory
    | (Const_int it) -> [op]::[STAR;IDENT i;NUMBER (-1)]::solve_unary_rest t memory
    | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use unary operators +/- on booleans")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use unary operators +/- on booleans")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | [(PLUS|MINUS|DIV|MOD|STAR|STARSTAR) as op]::[MINUS]::(x::y)::t -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> [op]::((STAR)::(NUMBER (-1))::x::y)::(solve_unary_rest t memory)
    | _ -> raise( Failure "Syntax Error - unary operators +/- can only be applied to integers")
    )          
  | [(PLUS|MINUS|DIV|MOD|STAR|STARSTAR) as op]::[PLUS]::[NUMBER n]::t -> [op]::[NUMBER n]::solve_unary_rest t memory         
  | [(PLUS|MINUS|DIV|MOD|STAR|STARSTAR) as op]::[PLUS]::[IDENT i]::t -> (match (get_type i memory) with
    | (Var_int it) -> [op]::[IDENT i]::solve_unary_rest t memory
    | (Const_int it) -> [op]::[IDENT i]::solve_unary_rest t memory
    | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use unary operators +/- on booleans")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use unary operators +/- on booleans")
    | (Var_int_nd)|(Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )         
  | [(PLUS|MINUS|DIV|MOD|STAR|STARSTAR) as op]::[PLUS]::(x::y)::t -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> [op]::(x::y)::(solve_unary_rest t memory)
    | _ -> raise( Failure "Syntax Error - unary operator can only be applied to integers")
    )
  | h::t -> h::solve_unary_rest t memory
  in
  match expr with
  | [] -> []
  | [PLUS]::[PLUS]::t -> let expr = (solve_unary t memory) in (solve_unary_rest expr memory)
  | [PLUS]::[MINUS]::t -> let expr = (solve_unary ([MINUS]::t) memory) in (solve_unary_rest expr memory)
  | [MINUS]::[PLUS]::t -> let expr = (solve_unary ([MINUS]::t) memory) in (solve_unary_rest expr memory)
  | [MINUS]::[MINUS]::t -> let expr = (solve_unary t memory) in (solve_unary_rest expr memory)
  | [PLUS]::[NUMBER n]::t -> let expr = ([NUMBER n]::t) in (solve_unary_rest expr memory)
  | [PLUS]::[IDENT i]::t -> (match (get_type i memory) with
    | (Var_int it) -> let expr = ([IDENT i]::t) in (solve_unary_rest expr memory)
    | (Const_int it) -> let expr = ([IDENT i]::t) in (solve_unary_rest expr memory)
    | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use unary operators +/- on booleans")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use unary operators +/- on booleans")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | [PLUS]::(x::y)::t -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> let expr = ((x::y)::t) in (solve_unary_rest expr memory)
    | _ -> raise( Failure "Syntax Error - unary operator can only be applied to integers")
    )
  | [MINUS]::[NUMBER n]::t -> let expr = ([STAR;NUMBER n;NUMBER (-1)]::t) in (solve_unary_rest expr memory)
  | [MINUS]::[IDENT i]::t -> (match (get_type i memory) with
    | (Var_int it) -> let expr = ([STAR;IDENT i;NUMBER (-1)]::t) in (solve_unary_rest expr memory)
    | (Const_int it) -> let expr = ([STAR;IDENT i;NUMBER (-1)]::t) in (solve_unary_rest expr memory)
    | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use unary operators +/- on booleans")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use unary operators +/- on booleans")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | [MINUS]::(x::y)::t -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> let expr = (((STAR)::(NUMBER (-1))::x::y)::t) in (solve_unary_rest expr memory)
    | _ -> raise( Failure "Syntax Error - unary operator can only be applied to integers")
    )
  | _ -> (solve_unary_rest expr memory);;

(*
  Encontra os operadores STAR|DIV|MOD|AND e converte a expressao para RPN (invertida)
  Solve for * | div | mod | and
*)  
let rec solve_multi expr memory =
match expr with
| [] -> []
| [NUMBER n]::[(STAR|DIV|MOD) as tok]::[NUMBER m]::t -> solve_multi ([tok;NUMBER n;NUMBER m]::t) memory
| [NUMBER n]::[(STAR|DIV|MOD) as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> solve_multi ([tok;NUMBER n;IDENT i]::t) memory
  | (Const_int it) -> solve_multi ([tok;NUMBER n;IDENT i]::t) memory
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [IDENT i]::[(STAR|DIV|MOD) as tok]::[NUMBER n]::t -> (match (get_type i memory) with
  | (Var_int it) -> solve_multi ([tok;IDENT i;NUMBER n]::t) memory
  | (Const_int it) -> solve_multi ([tok;IDENT i;NUMBER n]::t) memory
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [IDENT i]::[(STAR|DIV|MOD) as tok]::[IDENT j]::t -> (match (get_type i memory) with
  | (Var_int it) -> (match (get_type j memory) with
    | (Var_int it) -> solve_multi ([tok;IDENT i;IDENT j]::t) memory
    | (Const_int it) -> solve_multi ([tok;IDENT i;IDENT j]::t) memory
    | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Const_int it) -> (match (get_type j memory) with
    | (Var_int it) -> solve_multi ([tok;IDENT i;IDENT j]::t) memory
    | (Const_int it) -> solve_multi ([tok;IDENT i;IDENT j]::t) memory
    | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [NUMBER n]::[(STAR|DIV|MOD) as tok]::(x::y)::t -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_multi ((tok::(NUMBER n)::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - operators */div/mod can only be applied to integers")
    )
| (x::y)::[(STAR|DIV|MOD) as tok]::[NUMBER n]::t -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_multi (((tok::x::y)@[NUMBER n])::t) memory
    | _ -> raise( Failure "Syntax Error - operators */div/mod can only be applied to integers")
    )
| [IDENT i]::[(STAR|DIV|MOD) as tok]::(x::y)::t -> (match (get_type i memory) with
  | (Var_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_multi ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - operators */div/mod can only be applied to integers")
    )
  | (Const_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_multi ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - operators */div/mod can only be applied to integers")
    )
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[(STAR|DIV|MOD) as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_multi (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - operators */div/mod can only be applied to integers")
    )
  | (Const_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_multi (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - operators */div/mod can only be applied to integers")
    )
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators */div/mod on booleans")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[(STAR|DIV|MOD) as tok]::(k::l)::t -> (match x with
  | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (match k with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_multi (((tok::x::y)@(k::l))::t) memory
    | _ -> raise( Failure "Syntax Error - operators */div/mod can only be applied to integers")
    )
  | _ -> raise( Failure "Syntax Error - operators */div/mod can only be applied to integers")
  )

| [NUMBER n]::[AND as tok]::[NUMBER m]::t -> raise( Failure "Syntax Error - You cannot use operator and on integers")
| [NUMBER n]::[AND as tok]::[IDENT i]::t -> raise( Failure "Syntax Error - You cannot use operator and on integers")
| [IDENT i]::[AND as tok]::[NUMBER n]::t -> raise( Failure "Syntax Error - You cannot use operator and on integers")
| [IDENT i]::[AND as tok]::[IDENT j]::t -> (match (get_type i memory) with
  | (Var_int it) -> raise( Failure "Syntax Error - You cannot use operator and on integers")
  | (Const_int it) -> raise( Failure "Syntax Error - You cannot use operator and on integers")
  | (Var_bool bl) -> (match (get_type j memory) with
    | (Var_int it) -> raise( Failure "Syntax Error - You cannot use operator and on integers")
    | (Const_int it) -> raise( Failure "Syntax Error - You cannot use operator and on integers")
    | (Var_bool bl) -> solve_multi ([tok;IDENT i;IDENT j]::t) memory
    | (Const_bool bl) -> solve_multi ([tok;IDENT i;IDENT j]::t) memory
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Const_bool bl) -> (match (get_type j memory) with
    | (Var_int it) -> raise( Failure "Syntax Error - You cannot use operator and on integers")
    | (Const_int it) -> raise( Failure "Syntax Error - You cannot use operator and on integers")
    | (Var_bool bl) -> solve_multi ([tok;IDENT i;IDENT j]::t) memory
    | (Const_bool bl) -> solve_multi ([tok;IDENT i;IDENT j]::t) memory
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [NUMBER n]::[AND as tok]::(x::y)::t -> raise( Failure "Syntax Error - You cannot use operator and on integers")
| (x::y)::[AND as tok]::[NUMBER n]::t -> raise( Failure "Syntax Error - You cannot use operator and on integers")
| [IDENT i]::[AND as tok]::(x::y)::t -> (match (get_type i memory) with
  | (Var_int it) -> raise( Failure "Syntax Error - You cannot use operator and on integers")
  | (Const_int it) -> raise( Failure "Syntax Error - You cannot use operator and on integers")
  | (Var_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_multi ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - operators and can only be applied to integers")
    )
  | (Const_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_multi ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - operators and can only be applied to integers")
    )
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[AND as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> raise( Failure "Syntax Error - You cannot use operator and on integers")
  | (Const_int it) -> raise( Failure "Syntax Error - You cannot use operator and on integers")
  | (Var_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_multi (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - operators and can only be applied to integers")
    )
  | (Const_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_multi (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - operators and can only be applied to integers")
    )
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[AND as tok]::(k::l)::t -> (match x with
  | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> (match k with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_multi (((tok::x::y)@(k::l))::t) memory
    | _ -> raise( Failure "Syntax Error - operator and can only be applied to booleans")
    )
  | _ -> raise( Failure "Syntax Error - operator and can only be applied to booleans")
  )

| h::t -> h::(solve_multi t memory);;


(*
  Encontra os operadores PLUS|MINUS|OR e converte a expressao para RPN (invertida)
  Solve for + | - | or
*)  
let rec solve_sums expr memory =
match expr with
| [] -> []
| [NUMBER n]::[(PLUS|MINUS) as tok]::[NUMBER m]::t -> solve_sums ([tok;NUMBER n;NUMBER m]::t) memory
| [NUMBER n]::[(PLUS|MINUS) as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> solve_sums ([tok;NUMBER n;IDENT i]::t) memory
  | (Const_int it) -> solve_sums ([tok;NUMBER n;IDENT i]::t) memory
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [IDENT i]::[(PLUS|MINUS) as tok]::[NUMBER n]::t -> (match (get_type i memory) with
  | (Var_int it) -> solve_sums ([tok;IDENT i;NUMBER n]::t) memory
  | (Const_int it) -> solve_sums ([tok;IDENT i;NUMBER n]::t) memory
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [IDENT i]::[(PLUS|MINUS) as tok]::[IDENT j]::t -> (match (get_type i memory) with
  | (Var_int it) -> (match (get_type j memory) with
    | (Var_int it) -> solve_sums ([tok;IDENT i;IDENT j]::t) memory
    | (Const_int it) -> solve_sums ([tok;IDENT i;IDENT j]::t) memory
    | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Const_int it) -> (match (get_type j memory) with
    | (Var_int it) -> solve_sums ([tok;IDENT i;IDENT j]::t) memory
    | (Const_int it) -> solve_sums ([tok;IDENT i;IDENT j]::t) memory
    | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [NUMBER n]::[(PLUS|MINUS) as tok]::(x::y)::t -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_sums ((tok::(NUMBER n)::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - operators +/- can only be applied to integers")
    )
| (x::y)::[(PLUS|MINUS) as tok]::[NUMBER n]::t -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_sums (((tok::x::y)@[NUMBER n])::t) memory
    | _ -> raise( Failure "Syntax Error - operators +/- can only be applied to integers")
    )
| [IDENT i]::[(PLUS|MINUS) as tok]::(x::y)::t -> (match (get_type i memory) with
  | (Var_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_sums ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - operators +/- can only be applied to integers")
    )
  | (Const_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_sums ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - operators +/- can only be applied to integers")
    )
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[(PLUS|MINUS) as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_sums (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - operators +/- can only be applied to integers")
    )
  | (Const_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_sums (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - operators +/- can only be applied to integers")
    )
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use operators +/- on booleans")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[(PLUS|MINUS) as tok]::(k::l)::t -> (match x with
  | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (match k with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_sums (((tok::(x::y))@(k::l))::t) memory
    | _ -> raise( Failure "Syntax Error - operators +/- can only be applied to integers")
    )
  | _ -> raise( Failure "Syntax Error - operators +/- can only be applied to integers")
  )

| [NUMBER n]::[OR as tok]::[NUMBER m]::t -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
| [NUMBER n]::[OR as tok]::[IDENT i]::t -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
| [IDENT i]::[OR as tok]::[NUMBER n]::t -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
| [IDENT i]::[OR as tok]::[IDENT j]::t -> (match (get_type i memory) with
  | (Var_int it) -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
  | (Const_int it) -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
  | (Var_bool bl) -> (match (get_type j memory) with
    | (Var_int it) -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
    | (Const_int it) -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
    | (Var_bool bl) -> solve_sums ([tok;IDENT i;IDENT j]::t) memory
    | (Const_bool bl) -> solve_sums ([tok;IDENT i;IDENT j]::t) memory
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Const_bool bl) -> (match (get_type j memory) with
    | (Var_int it) -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
    | (Const_int it) -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
    | (Var_bool bl) -> solve_sums ([tok;IDENT i;IDENT j]::t) memory
    | (Const_bool bl) -> solve_sums ([tok;IDENT i;IDENT j]::t) memory
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [NUMBER n]::[OR as tok]::(x::y)::t -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
| (x::y)::[OR as tok]::[NUMBER n]::t -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
| [IDENT i]::[OR as tok]::(x::y)::t -> (match (get_type i memory) with
  | (Var_int it) -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
  | (Const_int it) -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
  | (Var_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_sums ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - operators or can only be applied to booleans")
    )
  | (Const_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_sums ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - operators or can only be applied to booleans")
    )
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[OR as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
  | (Const_int it) -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
  | (Var_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_sums (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - operators or can only be applied to booleans")
    )
  | (Const_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_sums (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - operators or can only be applied to booleans")
    )
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[OR as tok]::(k::l)::t -> (match x with
  | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> (match k with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_sums (((tok::x::y)@(k::l))::t) memory
    | _ -> raise( Failure "Syntax Error - operator or can only be applied to booleans")
    )
  | _ -> raise( Failure "Syntax Error - operators or can only be applied to booleans")
  )

| h::t -> h::(solve_sums t memory);;

(*
  Encontra os operadores EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT e converte a expressao para RPN (invertida)
  Solve for = | >= | > | <> | < | <=
*)  
let rec solve_comp expr memory =
match expr with
| [] -> []
| [NUMBER n]::[(EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) as tok]::[NUMBER m]::t -> solve_comp ([tok;NUMBER n;NUMBER m]::t) memory
| [NUMBER n]::[(EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> solve_comp ([tok;NUMBER n;IDENT i]::t) memory
  | (Const_int it) -> solve_comp ([tok;NUMBER n;IDENT i]::t) memory
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [IDENT i]::[(EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) as tok]::[NUMBER n]::t -> (match (get_type i memory) with
  | (Var_int it) -> solve_comp ([tok;IDENT i;NUMBER n]::t) memory
  | (Const_int it) -> solve_comp ([tok;IDENT i;NUMBER n]::t) memory
  | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [IDENT i]::[(EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) as tok]::[IDENT j]::t -> (match (get_type i memory) with
  | (Var_int it) -> (match (get_type j memory) with
    | (Var_int it) -> solve_comp ([tok;IDENT i;IDENT j]::t) memory
    | (Const_int it) -> solve_comp ([tok;IDENT i;IDENT j]::t) memory
    | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Const_int it) -> (match (get_type j memory) with
    | (Var_int it) -> solve_comp ([tok;IDENT i;IDENT j]::t) memory
    | (Const_int it) -> solve_comp ([tok;IDENT i;IDENT j]::t) memory
    | (Var_bool bl) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Var_bool bl) -> (match (get_type j memory) with
    | (Var_int it) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    | (Const_int it) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    | (Var_bool bl) -> solve_comp ([tok;IDENT i;IDENT j]::t) memory
    | (Const_bool bl) -> solve_comp ([tok;IDENT i;IDENT j]::t) memory
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Const_bool bl) -> (match (get_type j memory) with
    | (Var_int it) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    | (Const_int it) -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    | (Var_bool bl) -> solve_comp ([tok;IDENT i;IDENT j]::t) memory
    | (Const_bool bl) -> solve_comp ([tok;IDENT i;IDENT j]::t) memory
    | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
    )
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| [NUMBER n]::[(EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) as tok]::(x::y)::t -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_comp ((tok::(NUMBER n)::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
| (x::y)::[(EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) as tok]::[NUMBER n]::t -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_comp (((tok::x::y)@[NUMBER n])::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
| [IDENT i]::[(EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) as tok]::(x::y)::t -> (match (get_type i memory) with
  | (Var_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_comp ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
  | (Const_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_comp ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
  | (Var_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_comp ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
  | (Const_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_comp ((tok::IDENT i::x::y)::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[(EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) as tok]::[IDENT i]::t -> (match (get_type i memory) with
  | (Var_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_comp (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
  | (Const_int it) -> (match x with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_comp (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
  | (Var_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_comp (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
  | (Const_bool bl) -> (match x with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_comp (((tok::x::y)@[IDENT i])::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
  | (Var_int_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  | (Var_bool_nd) -> raise( Failure "Syntax Error - Invalid use of undefined variable")
  )
| (x::y)::[(EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) as tok]::(k::l)::t -> (match x with
  | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> (match k with
    | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR) -> solve_comp (((tok::x::y)@(k::l))::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
  | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> (match k with
    | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> solve_comp (((tok::x::y)@(k::l))::t) memory
    | _ -> raise( Failure "Syntax Error - You cannot use comparison operators on diferent types")
    )
  | _ -> raise( Failure "Syntax Error - not an arithmetic operator")
  )

| h::t -> h::(solve_comp t memory);;

(*
  Para operar sobre tuplos... 
  To operate over tuples...  
*)

let first (x,y) = x;;
let second (x,y) = y;;

(*
  retira uma expressao entre parentesis tendo em conta a possivel existencia de outras expressoes dentro desta
  Extracts an expression between parenthesis
*)
let rec take_sub_list n expr =
match expr with
| [] -> raise( Failure "Syntax Error - ) missing")
| [RPAREN]::t -> let n=n-1 in
  if n=0 then ([],t) else ([RPAREN]::first((take_sub_list n t)),second((take_sub_list n t)))
| [LPAREN]::t -> let n=n+1 in
  ([LPAREN]::first((take_sub_list n t)),second((take_sub_list n t)))
| h::t -> (h::first((take_sub_list n t)),second((take_sub_list n t)));;   

(*
  resolve as expressoes transformando-as em notaçao RPN (tokens list list)
  Builds up the RPN expressions
*)
let rec make_rpn_expr expr memory =
  let rec solve_paren expr memory =
  match expr with
  | [] -> []
  | [LPAREN]::t -> (make_rpn_expr (first(take_sub_list 1 t)) memory)@(solve_paren (second(take_sub_list 1 t)) memory)
  | h::t -> h::(solve_paren t memory) 
  in
  (solve_comp (solve_sums (solve_multi (solve_starstar (solve_not (solve_unary (solve_paren expr memory) memory) memory) memory) memory) memory) memory);;


(*
  filtra a expressao anterior por forma a detectar eventuais erros e simplificar o seu uso
  Detects errors in the expressions
*)
let filter_expr expr =
match expr with
| [] -> raise( Failure "Syntax Error - Arithmetic expression expected")
| [h] -> h
| (h::t)::z -> raise( Failure "Syntax Error - Bad arithmetic expression");;


(*
  Converte uma stream para lista até encontrar um dos dois elementos de paragem, devolve o elemento encontrado e a lista
  Converts a stream to a list until one of 2 stop elements is found, returns the element found and the processed list
*)
let rec list_of_stream_dual stop1 stop2 lista i =
match i with
| [< 'tok ; r>] -> if tok=stop1 then (stop1,lista)
                   else (
                   if tok=stop2 then (stop2,lista) 
                   else (list_of_stream_dual stop1 stop2 (lista@[[tok]]) r)
                   ) 
| [< >] -> raise( Failure "Syntax Error - Unexpected end");;



(*
  Resolve expressoes para os 'assignments' confirmando o tipo até encontrar 1 de 2 elementos de paragem
  (value -> tokens -> tokens -> (allocation list*tokens list arvore) -> (tokens stream)) -> (tokens -> tokens list -> (allocation list*tokens list arvore))
  
  Solves assignment expressions checking the type until one of 2 stop elements is found.
*)

let solve_type_expression id stop1 stop2 rip r =
let (stop,lista) = (list_of_stream_dual stop1 stop2 [] r) in let expr = filter_expr( make_rpn_expr lista (first rip)) in
  let x = (match expr with
    | h::t -> h
    | _ -> raise( Failure "Internal error")
    )  in
  (match x with
  | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> (match (get_type id (first rip)) with
    | (Var_int it) -> raise( Failure "Syntax Error - Assignments can only be made between same type")
    | (Const_int it) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
    | (Var_bool bl) -> (stop,expr,rip)
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Assignments can only be made between same type") 
    | (Var_bool_nd) -> (stop,expr,((put_value id (first rip)),(second rip)))
    )
  | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR)  -> (match (get_type id (first rip)) with
    | (Var_int it) -> (stop,expr,rip)
    | (Const_int it) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
    | (Var_bool bl) -> raise( Failure "Syntax Error - Assignments can only be made between same type")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
    | (Var_int_nd) -> (stop,expr,((put_value id (first rip)),(second rip)))
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Assignments can only be made between same type")
    )
  | (NUMBER nm) -> (match (get_type id (first rip)) with
    | (Var_int it) -> (stop,expr,rip)
    | (Const_int it) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
    | (Var_bool bl) -> raise( Failure "Syntax Error - Assignments can only be made between same type")
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
    | (Var_int_nd) -> (stop,expr,((put_value id (first rip)),(second rip)))
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Assignments can only be made between same type") 
    )
  | (IDENT str) -> (match (get_type id (first rip)) with
    | (Var_int it) -> (match (get_type str (first rip)) with
      | (Var_int it) -> (stop,expr,rip)
      | (Const_int it) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
      | (Var_bool bl) -> raise( Failure "Syntax Error - Assignments can only be made between same type")
      | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
      | (Var_int_nd) -> raise( Failure "Internal error")
      | (Var_bool_nd) -> raise( Failure "Internal error")
      )
    | (Const_int it) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
    | (Var_bool bl) -> (match (get_type str (first rip)) with 
      | (Var_int it) -> raise( Failure "Syntax Error - Assignments can only be made between same type")
      | (Const_int it) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
      | (Var_bool bl) -> (stop,expr,rip)
      | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
      | (Var_int_nd) -> raise( Failure "Internal error")
      | (Var_bool_nd) -> raise( Failure "Internal error")
      )
    | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
    | (Var_int_nd) -> (match (get_type str (first rip)) with
      | (Var_int it) -> (stop,expr,((put_value id (first rip)),(second rip)))
      | (Const_int it) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
      | (Var_bool bl) -> raise( Failure "Syntax Error - Assignments can only be made between same type")
      | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
      | (Var_int_nd) -> raise( Failure "Internal error")
      | (Var_bool_nd) -> raise( Failure "Internal error")
      )
    | (Var_bool_nd) -> (match (get_type str (first rip)) with 
      | (Var_int it) -> raise( Failure "Syntax Error - Assignments can only be made between same type")
      | (Const_int it) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
      | (Var_bool bl) -> (stop,expr,((put_value id (first rip)),(second rip)))
      | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
      | (Var_int_nd) -> raise( Failure "Internal error")
      | (Var_bool_nd) -> raise( Failure "Internal error")
      )
    )
  | _ -> raise( Failure "Internal Error")
  );;


(*
  Resolve expressoes booleanas até encontrar 1 de 2 elementos de paragem
  ((tokens -> tokens -> (allocation list*tokens list arvore) -> (tokens stream)) -> (tokens -> tokens list -> rip))
  
  Solves boolean expressions until one of 2 stop elements is found.
*)
let solve_boolean_expression stop1 stop2 rip r =
let (stop,lista) = (list_of_stream_dual stop1 stop2 [] r) in let expr = filter_expr( make_rpn_expr lista (first rip)) in
  let x = (match expr with
    | h::t -> h
    | _ -> raise( Failure "Internal error")
    )  in
  (match x with
  | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> (stop,expr,rip)
  | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR)  -> raise( Failure "Syntax Error - Boolean expression expected")
  | (NUMBER nm) -> raise( Failure "Syntax Error - Boolean expression expected")
  | (IDENT str) -> (match (get_type str (first rip)) with
    | (Var_int it) -> raise( Failure "Syntax Error - Boolean expression expected")
    | (Const_int it) -> raise( Failure "Syntax Error - Boolean expression expected")
    | (Var_bool bl) -> (stop,expr,rip)
    | (Const_bool bl) -> (stop,expr,rip)
    | (Var_int_nd) -> raise( Failure "Syntax Error - Value undefined")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Value undefined")
    )
  | _ -> raise( Failure "Internal Error")
  );;

(*
  Resolve expressoes de inteiros até encontrar 1 de 2 elementos de paragem
  ((tokens -> tokens -> (allocation list*tokens list arvore) -> (tokens stream)) -> (tokens -> tokens list -> rip))
  
  Solves integer expressions until one of 2 stop elements is found.
*)
let solve_integer_expression stop1 stop2 rip r =
let (stop,lista) = (list_of_stream_dual stop1 stop2 [] r) in let expr = filter_expr( make_rpn_expr lista (first rip)) in
  let x = (match expr with
    | h::t -> h
    | _ -> raise( Failure "Internal error")
    )  in
  (match x with
  | (AND|OR|NOT|EQUAL|GE|GT|EQUAL|NOTEQUAL|LE|LT) -> raise( Failure "Syntax Error - Integer expression expected")
  | (PLUS|MINUS|DIV|MOD|STAR|STARSTAR)  -> (stop,expr,rip)
  | (NUMBER nm) -> (stop,expr,rip)
  | (IDENT str) -> (match (get_type str (first rip)) with
    | (Var_int it) -> (stop,expr,rip)
    | (Const_int it) -> (stop,expr,rip)
    | (Var_bool bl) -> raise( Failure "Syntax Error - Boolean expression expected")
    | (Const_bool bl) -> raise( Failure "Syntax Error - Boolean expression expected")
    | (Var_int_nd) -> raise( Failure "Syntax Error - Value undefined")
    | (Var_bool_nd) -> raise( Failure "Syntax Error - Value undefined")
    )
  | _ -> raise( Failure "Internal Error")
  );;


(*
  Resolve blocos de instruções
  Solves instruction blocks
*)
let rec solve_block stop rip i =
(*
  Resolve uma instruçao apenas
  Solves for an instruction
*)
let rec solve_single stop1 stop2 rip i =
match i with
| [< '(IDENT id); 'ASSIGNMENT; r >] -> let (stop,expr,rip) = (solve_type_expression id stop1 stop2 rip i) in
                                       (stop,((first rip),No ([ASSIGNMENT],[No ([IDENT id],[]);No (expr,[])])))
| [< '(IF); r>] -> let (stop,expr,rip) = (solve_boolean_expression THEN THEN rip r) in
                   let (stopped, rip) = (solve_single SEMICOLON ELSE rip r) in
                   if stopped = SEMICOLON then (stopped,((first rip),No ([IF],[No (expr,[]);(second rip);No ([],[])])))
                   else let (stopped, rip2) = (solve_single stop1 stop2 rip i) in (stopped,((first rip2),No ([IF],[ No (expr,[]);(second rip);(second rip2)])))
| [< 'FOR; '(IDENT id); 'ASSIGNMENT; r >] -> let rip = ((put_value id (first rip)), (second rip)) in (match (get_type id (first rip)) with
  | (Var_int it) -> (let (stopto,expr,rip) = (solve_integer_expression TO DOWNTO rip r) in
                                             let (stop,expr2,rip) = (solve_integer_expression DO DO rip r) in
                                             let (stopped,rip) = (solve_single stop1 stop2 rip r) in
                                             if (stopto = TO) then (stop,((first rip) ,No ([FOR],[No ([IDENT id],[]);No (expr,[]); No (expr2,[]); (second rip)])))
                                             else (stop,((first rip) ,No ([FOR],[No ([IDENT id],[]);No (expr2,[]); No (expr,[]); (second rip)]))))
  | (Const_int it) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
  | (Var_bool bl) -> raise( Failure "Syntax Error - The for cycle control variable can be only an integer")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
  | (Var_int_nd) -> raise( Failure "Internal error")
  | (Var_bool_nd) -> raise( Failure "Internal error")
  )                                             
| [< 'REPEAT; r >] -> let rip = (solve_block UNTIL rip r) in
                      let (stop,expr,rip2) = (solve_boolean_expression SEMICOLON SEMICOLON rip r) in
                      (stop,((first rip2),No ([REPEAT],[No (expr,[]);(second rip)])))
| [< 'WHILE; r >] -> let (stp,expr,rip) = (solve_boolean_expression DO DO rip r) in
                     let (stop,rip) = (solve_single stop1 stop2 rip r) in (stop,((first rip),No ([WHILE],[No (expr,[]);(second rip)])))
| [< 'BEGIN; r >] -> let rip = (solve_block END rip r) in
                     (match r with
                     | [< 'tok >] -> if tok = stop1 then (stop1,((first rip), No ([BEGIN],[(second rip)])))
                                     else if tok = stop2 then (stop2,((first rip), No ([BEGIN],[(second rip)])))
				     else raise( Failure "Syntax Error - Unexpected token")
                     | [< >] -> raise( Failure "Syntax Error - Unexpected end")
                     )

| [< '(WRITE|WRITELN) as tok; r >] -> let rec track_write tree_list str stop1 stop2 rip =
  (match str with
  | [< 'RPAREN; 'stopped ; r >] -> if stopped=stop1 then (stop1,tree_list)
                                   else (if stopped=stop2 then (stop2,tree_list)
                                         else raise( Failure "Syntax Error - Unexpected error"))  
  | [< '(STRING n) as addtok; r >] -> (match r with
      | [< 'COMMA; r >] -> (track_write (tree_list@[No ([addtok],[])]) r stop1 stop2 rip)
      | [< 'RPAREN; 'stopped ; r >] -> if stopped=stop1 then (stop1, tree_list@[No ([addtok],[])])
                                       else (if stopped=stop2 then (stop2, tree_list@[No ([addtok],[])])
                                             else raise( Failure "Syntax Error - Unexpected error"))
      | [< >] -> raise( Failure "Syntax Error - Unexpected token")
      )
  | [< r >] -> let (stp,lista) = (list_of_stream_dual COMMA RPAREN [] r) in
               let expr = filter_expr( make_rpn_expr lista (first rip)) in
               if (stp = COMMA) then
                 (track_write (tree_list@[No (expr,[])]) r stop1 stop2 rip)
               else
                 (match r with
                   | [< 'stopped; r >] -> if stopped=stop1 then (stop1,[No (expr,[])])
                                        else (if stopped=stop2 then (stop2,[No (expr,[])])
                                              else raise( Failure "Syntax Error - Unexpected error"))
                   | [< >] -> raise( Failure "Syntax Error - Unexpected token")
                   )
  | [< >] -> raise( Failure "Syntax Error - Unexpected end")
  ) in
  (match r with
  | [< 'LPAREN; r >] ->  let (stop,tree_list) = (track_write [] r stop1 stop2 rip) in
    (stop, ((first rip), No ([tok],tree_list@[Vazia])))
  | [< 'stopped; r >] -> if stopped=stop1 then (stop1,((first rip), No ([tok],[Vazia]))) 
                         else (if stopped=stop2 then (stop1,((first rip), No ([tok],[Vazia]))) 
                              else raise( Failure "Syntax Error - Unexpected error"))
  | [< >] -> raise( Failure "Syntax Error - Unexpected token")
  )

| [< >] -> raise( Failure "Syntax Error - Unexpected error")                     
in

match i with
| [< '(IDENT id); 'ASSIGNMENT; r >] -> let (stp,expr,rip) = (solve_type_expression id SEMICOLON SEMICOLON rip i) in
                                       let rip = (solve_block stop rip r) in
                                       ((first rip),No ([ASSIGNMENT],[No ([IDENT id],[]);No (expr,[]); (second rip)]))
| [< '(IF); r>] -> let (stp,expr,rip) = (solve_boolean_expression THEN THEN rip r) in
                   let (stopped, rip) = (solve_single SEMICOLON ELSE rip r) in
                   if stopped = SEMICOLON then let rip2 = (solve_block stop rip r) in ((first rip2),No ([IF],[No (expr,[]);(second rip);No ([],[]);(second rip2)]))
                   else let (stopped, rip2) = (solve_single SEMICOLON SEMICOLON rip i) in
                        let rip3 = (solve_block stop rip2 r) in ((first rip3),No ([IF],[ No (expr,[]);(second rip);(second rip2);(second rip3)]))
| [< 'FOR; '(IDENT id); 'ASSIGNMENT; r >] -> let rip = ((put_value id (first rip)), (second rip)) in (match (get_type id (first rip)) with
  | (Var_int it) -> (let (stpto,expr,rip) = (solve_integer_expression TO DOWNTO rip r) in
                                             let (stp,expr2,rip) = (solve_integer_expression DO DO rip r) in
                                             let (stopped,rip) = (solve_single SEMICOLON SEMICOLON rip r) in
                                             let rip2 = (solve_block stop rip r) in
                                             if (stpto = TO) then ((first rip2) ,No ([FOR],[No ([IDENT id],[]);No (expr,[]); No (expr2,[]); (second rip); (second rip2)]))
                                             else ((first rip2) ,No ([FOR],[No ([IDENT id],[]);No (expr2,[]); No (expr,[]); (second rip); (second rip2)])))
  | (Const_int it) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
  | (Var_bool bl) -> raise( Failure "Syntax Error - for control variable can only be an integer")
  | (Const_bool bl) -> raise( Failure "Syntax Error - You cannot assign a value to an constant")
  | (Var_int_nd) -> raise( Failure "Internal error")
  | (Var_bool_nd) -> raise( Failure "Internal error")
  )                                             
| [< 'REPEAT; r >] -> let rip = (solve_block UNTIL rip r) in
                      let (stp,expr,rip2) = (solve_boolean_expression SEMICOLON SEMICOLON rip r) in
                      let rip3 = (solve_block stop rip2 r) in ((first rip3),No ([REPEAT],[No (expr,[]);(second rip);(second rip3)]))
| [< 'WHILE; r >] -> let (stp,expr,rip) = (solve_boolean_expression DO DO rip r) in
                     let (stopped,rip) = (solve_single SEMICOLON SEMICOLON rip r) in
                     let rip2 = (solve_block stop rip r) in ((first rip2),No ([WHILE],[No (expr,[]);(second rip);(second rip2)]))
| [< 'BEGIN; r >] -> let rip = (solve_block END rip r) in
                     (match r with
                     | [< 'tok >] -> if tok = SEMICOLON then let rip2 = (solve_block stop rip r) in ((first rip2), No ([BEGIN],[(second rip);(second rip2)]))
                                     else raise( Failure "Syntax Error - Unexpected token")
                     | [< >] -> raise( Failure "Syntax Error - Unexpected end")
                     )

| [< '(WRITE|WRITELN) as tok; r >] -> let rec track_write tree_list str rip =
  (match str with
  | [< 'RPAREN; 'SEMICOLON ; r >] -> tree_list
  | [< '(STRING n) as addtok; r >] -> (match r with
      | [< 'COMMA; r >] -> (track_write (tree_list@[No ([addtok],[])]) r rip)
      | [< 'RPAREN; 'SEMICOLON ; r >] -> tree_list@[No ([addtok],[])]
      | [< >] -> raise( Failure "Syntax Error - Unexpected token")
      )
  | [< r >] -> let (stp,lista) = (list_of_stream_dual COMMA RPAREN [] r) in
               let expr = filter_expr( make_rpn_expr lista (first rip)) in
               if (stp = COMMA) then
                 (track_write (tree_list@[No (expr,[])]) r rip)
               else
                 (match r with
                   | [< 'SEMICOLON; r >] -> tree_list@[No (expr,[])]
                   | [< >] -> raise( Failure "Syntax Error - ; expected")
                   )
  | [< >] -> raise( Failure "Syntax Error - Unexpected end")
  ) in
  (match r with
  | [< 'SEMICOLON; r >] -> let rip = (solve_block stop rip r) in ((first rip), No ([tok],[second rip])) 
  | [< 'LPAREN; r >] ->  let tree_list = (track_write [] r rip) in
    let rip = (solve_block stop rip r) in
    ((first rip), No ([tok],tree_list@[second rip]))
  | [< >] -> raise( Failure "Syntax Error - Unexpected token")
  )

| [< 'tok >] -> if tok = stop then ((first rip),Vazia) else raise( Failure "Syntax Error - Unexpected token")
| [< >] -> raise( Failure "Syntax Error - Unexpected end");;                     


(*
  Verifica se um dado elemento de se encontra já na lista
  Checks if a given element is already in the list
*)
let rec is_in_list element list =
match list with
|[] -> false
|h::t -> (match h with
  | {Nome = x; Value = y} -> if x = element then true else is_in_list element t);;

(*
  Insere os elementos duma lista que representa o nome de variaveis booleanas, foi modificado para inicializar
  automaticamente a false
  
  Inserts the elements of a list with the name representation of boolean variables, was modified to initialize
  automatically as false
 *)
let rec allocate_list_var_bool list rip =
match list with
|[] -> rip
|h::t -> (match rip with
  |(x,y) -> if (is_in_list h x)
              then raise( Failure "Syntaxe Error - Double declaration")
              else let rip = (({Nome = h; Value = Var_bool false}::x),y) in allocate_list_var_bool t rip
  );;

(* 
  Insere os elementos duma lista que representa o nome de variaveis inteiras, foi modificado para inicializar
  automaticamente a zero
  
  Inserts the elements of a list with the name representation of integer variables, was modified to initialize
  automatically as zero
*)
let rec allocate_list_var_int list rip =
match list with
|[] -> rip
|h::t -> (match rip with
  |(x,y) -> if (is_in_list h x)
              then raise( Failure "Syntaxe Error - Double declaration")
              else let rip = (({Nome = h; Value = Var_int 0}::x),y) in allocate_list_var_int t rip
  );;

(*
  Detecta o nome das variaveis pondo-os numa lista destinguindo no fim o tipo de variavel
  
  Detects the variables' name putting them in a list and discriminating at the end the variable type.
*)
let rec allocate_list_var list rip r =
try
match r with
| [< 'COMMA; '(IDENT str); r >] -> (allocate_list_var (str::list) rip r) 
| [< 'COLON; 'tok ; 'SEMICOLON >] -> (match tok with
  | INTEGER -> allocate_list_var_int list rip
  | BOOLEAN -> allocate_list_var_bool list rip
  | _ -> raise( Failure "Syntaxe Error - type expected after a colon")
  )
| [< >] -> raise( Failure "Syntaxe Error - Comma or colon expected")
with Parse_error -> raise( Failure "Syntaxe Error - Comma followed by ident or colon followed by type expected");;

(*
  Insere os elementos de constantes booleanas na rip (o nome list provem de um erro encontrado e corrigido no algoritmo)

  Inserts the elements of boolean constants in the rip
*)
let rec allocate_list_const_bool valor str rip =
match rip with
  |(x,y) -> if (is_in_list str x)
              then raise( Failure "Syntaxe Error - Double declaration")
              else (match valor with
    | "true" -> (({Nome = str; Value = Const_bool true}::x),y)
    | "false" -> (({Nome = str; Value = Const_bool false}::x),y)
    | _ -> raise( Failure "Syntaxe Error - only boolean identifiers are allowed")
    );;

(*
  Insere os elementos de constantes inteiras na rip.
  
  Inserts the elements of integer constants in the rip.
*)
let rec allocate_list_const_int valor str rip =
match rip with
  |(x,y) -> if (is_in_list str x)
              then raise( Failure "Syntaxe Error - Double declaration")
              else (({Nome = str; Value = Const_int valor}::x),y);;

(*
  Insere elementos constantes, destinguindo inteiros de booleanos
  
  Inserts the constant elements, discriminating integers from booleans  
*)
let rec allocate_list_const str rip r =
match r with
| [< 'EQUAL; 'tok ; 'SEMICOLON >] -> (match tok with
  | NUMBER valor -> allocate_list_const_int valor str rip
  | IDENT valor -> allocate_list_const_bool valor str rip
  | _ -> raise( Failure "Syntaxe Error - a valid value followed by semicolon is expected after equal")
  )
| [< >] -> raise( Failure "Syntaxe Error - equal expected after a const identifier");;

(*
  analiza toda a parte declarativa do programa
  
  Analizes the declarative part of the program.
*)
let rec analize_declaration_part info rip r =
match info with
| VAR -> (match r with
  | [< '(IDENT str); r >] -> (allocate_list_var [str] rip r)
  | [< >] -> raise( Failure "Syntaxe Error - ident is expected")
  )
| CONST -> (match r with
  | [< '(IDENT str); r >] -> (allocate_list_const str rip r)
  | [< >] -> raise( Failure "Syntaxe Error - ident is expected")
  )
| _ -> raise( Failure "Internal Error on ASIN");;

(*
  analiza todo o programa redirecionando o processo para outras funcoes

  Analizes the whole program delegating the actual work to other functions.
*)
let rec analize_program rip i =
match i with
| [< 'VAR as info; r>] -> let rip = (analize_declaration_part info rip r) in analize_program rip r
| [< 'CONST as info; r>] -> let rip = (analize_declaration_part info rip r) in analize_program rip r
| [< 'BEGIN; r >] -> let rip = solve_block END rip r in
  (match r with
  | [< 'DOT; r >] -> (match r with
                   | [< 'tok >] -> raise( Failure "Syntax Error - End of file expected")
                   | [< >] -> rip
                   )
  | [< >] -> raise( Failure "Syntax Error - dot expected")
  )   
| [< >] -> raise( Failure "Syntax Error - begin expected");;

(*
  dada uma stream de tokens

  A token stream
*)
let asin i =
let rec filter_comments i =
match i with
| [< 'COMMENT; r >] -> filter_comments r
| [< 'tok; r >] -> [< 'tok; (filter_comments r) >]
| [< >] -> i
in
let i = (filter_comments i) in
let rip = ([], Vazia) in
match i with
| [< 'PROGRAM; 'IDENT str; 'SEMICOLON; r >] -> analize_program rip r
| [< >] -> raise( Failure "Syntax Error - program expected" );;

