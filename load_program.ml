include "alex";;
include "asin";;

(*
  carrega um program em pascal, lendo linha a linha para uma stream, devolve a rip para o run_program ou stat_program 
  load a Pascal program, reading line by line to a stream, returns a rip for run_program or stat_program 
*)

let load_program filename =
(* read a txt file and convert it to a stream *)
let rec read_file filename =
  let rec rall f s =
    try
      rall f (s^(input_line f)^"\n")
    with End_of_file -> (close_in f); s
  in rall (open_in filename) ""
in

let mystream = stream_of_string (read_file filename) in

let alex_stream = alex mystream in

asin alex_stream;;
