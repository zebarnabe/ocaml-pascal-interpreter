(* just for debug proposes *)
let next = function [< 'x >] -> x;;

(* tokens definition *)
type tokens =
 | AND 
 | GE
 | GT
 | PROGRAM
 | ASSIGNMENT
 | REPEAT
 | COLON
 | IF
 | RPAREN
 | COMMA
 | LE
 | SEMICOLON
 | CONST
 | LPAREN
 | SLASH
 | DIV
 | LT 
 | STAR
 | DO
 | MINUS 
 | STARSTAR
 | DOT
 | MOD
 | THEN
 | DOWNTO
 | NOT
 | TO
 | ELSE
 | NOTEQUAL
 | UNTIL
 | END
 | OR
 | VAR
 | EQUAL
 | BEGIN
 | WHILE
 | FOR
 | PLUS

 | STRING of string 
 | IDENT of string
 | NUMBER of int
 | COMMENT
 | WRITE
 | WRITELN
 | INTEGER
 | BOOLEAN;;


(* detect variables (or const) identifiers *)
let rec detect_variables str i =
match i with
  | [< '(`a`..`z` | `A`..`Z` | `0`..`9` | `_`) as c ;(detect_variables(str^(string_of_char c))) s >] -> s
  | [< >] -> IDENT str;;  

(* detect letters sequences, distinguish them in case of identifiers *)
let rec detect_letters str i =
match i with
  | [< '(`a`..`z` | `A`..`Z`) as c ; s >] -> (detect_letters (str^(string_of_char c)) s)
  | [< '(`0`..`9` | `_`) as c ;(detect_variables(str^(string_of_char c))) s >] -> s
  | [< >] -> (match str with
    | "and" -> AND
    | "const" -> CONST
    | "div" -> DIV
    | "do" -> DO
    | "downto" -> DOWNTO
    | "else" -> ELSE
    | "end" -> END
    | "for" -> FOR
    | "if" -> IF
    | "mod" -> MOD
    | "not" -> NOT
    | "or" -> OR
    | "begin" -> BEGIN
    | "program" -> PROGRAM
    | "repeat" -> REPEAT
    | "then" -> THEN
    | "to" -> TO
    | "until" -> UNTIL
    | "var" -> VAR    
    | "while" -> WHILE
    | "writeln" -> WRITELN
    | "write" -> WRITE
    | "integer" -> INTEGER 
    | "boolean" -> BOOLEAN
    | _ -> IDENT str);;  

(* take a digit as char and turn it into a integer *)
let int_of_digit i =
match i with
  | `0`..`9` as c -> (int_of_char c) - (int_of_char `0`)
  | _ -> raise (Failure "Error: internal error on int_of_digit");;

(* take a sequence of chars and turn them into a integer *)
let rec detect_numbers n i =
match i with
  | [< '(`0`..`9`) as c ;(detect_numbers(10*n + int_of_digit c)) s >] -> s
  | [< >] -> NUMBER n;;  

(* distinguish an assignment from an equalitty *)
let rec detect_assign i =
match i with
  | [< '`=` as c >] -> ASSIGNMENT 
  | [<  >] -> COLON;;

(* distinguish greater from greater or equal *)
let rec detect_great i =
match i with
  | [< '(`=`) as c >] -> GE 
  | [<  >] -> GT;;

(* distinguish less from less, not equal or equal *)
let rec detect_less i =
match i with
  | [< '(`=`) as c >] -> LE
  | [< '(`>`) as c >] -> NOTEQUAL
  | [<  >] -> LT;;

(* ignore comments made of { } *)
let rec ignore_comment_1 i =
try
match i with
  | [< '`}` >] -> COMMENT
  | [< 'a; r >] -> ignore_comment_1 r
with Parse_failure -> COMMENT;;

(* ignore comments made of (* *) *)
let rec ignore_comment_2 str i =
try
match str with
  | "*" -> (match i with
    | [< '`)` >] -> COMMENT
    | [< 'a; r >] -> (ignore_comment_2 (string_of_char a) r))
  | _ -> (match i with
    | [< '`*`; r >] -> (ignore_comment_2 "*" r)
    | [< 'a ; r >] -> (ignore_comment_2 "" r))
with Parse_failure -> COMMENT;;

(* detect comments distinguish them and left parentesis *)
let rec detect_comment str i =
try
match str with
  | "{" -> (ignore_comment_1 i)
  | "(" -> (match i with
    | [< '`*`; r >] -> (ignore_comment_2 "" r)
    | [< >] -> LPAREN)
  | _ -> raise (Failure "Internal Error on detect_comment")
with Parse_failure -> COMMENT;;


(* distinguish star from starstar *)
let rec detect_star i =
match i with
  | [< '(`*`) >] -> STARSTAR 
  | [<  >] -> STAR;;

(* symbol detection *)
let rec detect_symbols str i =
match str with
  | ":" -> (detect_assign i)
  | ">" -> (detect_great i)
  | "<" -> (detect_less i)
  | ("("|"{") as c -> (detect_comment c i)
  | "*" -> (detect_star i)
  | "=" -> EQUAL
  | "," -> COMMA
  | "." -> DOT
  | "-" -> MINUS
  | "+" -> PLUS
  | ")" -> RPAREN
  | ";" -> SEMICOLON
  | "\\" -> SLASH
  | _ -> raise (Failure "Internal Error on detect_symbols");;

(* string detection *)
let rec detect_string str i =
match i with
  | [< '`'` >] -> STRING str
  | [< 'a; i >] -> (detect_string (str^(string_of_char a)) i);;

(* performs all needed detections and conversions *)
let rec alex mystream = 
try
(match mystream with
  | [< '(` `|`\n`|`\r`|`\t`); alex r >] -> [< r >] 
  | [< '(`a`..`z`|`A`..`Z`) as c; (detect_letters (string_of_char c)) i; alex r >] -> [<'i; r>]
  | [< '(`0`..`9`) as c; (detect_numbers (int_of_digit c)) i; alex r >] -> [<'i; r>]
  | [< '(`:`|`=`|`,`|`.`|`>`|`<`|`(`|`-`|`+`|`)`|`;`|`\\`|`*`|`{`) as c; (detect_symbols (string_of_char c)) i; alex r >] -> [<'i; r>]
  | [< '`'` as c; (detect_string "") i; alex r >] -> [<'i; r >]
  | [< >] -> [< >])
with Parse_failure -> [< >];;
