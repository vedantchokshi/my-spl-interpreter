open Lang
open Lexer
open Parser
open Arg
	
let parse_input() = 
	let found_int = ref false in
    let num = ref 1 in
	while not !found_int do
		let line = read_line () in
		try
			set_output_num (int_of_string line);
			set_star_to_gen_from_langs (int_of_string line);
			found_int := true
		with e ->
			let string_list = string_to_list line in
				assign_var ("lang" ^ (string_of_int !num)) string_list;
				num := !num + 1
	done;;

let _ = 
	parse_input();
	try
		let input = open_in (Sys.argv.(1)) in
		while true do
			let line = input_line input in
			try
				let lexbuf = Lexing.from_string line  in  
				let line_to_eval = Parser.main Lexer.lang lexbuf in
				eval line_to_eval; 
				flush stdout 
			with 
				Parsing.Parse_error -> print_endline "Error: Programme cannot be parsed. Incorrect programme input"
				| End_of_file -> ()
		done
	with End_of_file -> () ;;	