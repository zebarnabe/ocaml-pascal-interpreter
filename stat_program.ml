let stat_program rip =
let memoria = (first rip) in
let rec printallnames memoria = 
match memoria with
| [] -> ()
| {Nome = n; Value = v}::t -> print_string (n^"\n"); printallnames t
in
printallnames memoria;;