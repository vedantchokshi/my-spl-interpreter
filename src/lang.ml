open Hashtbl;;

exception InvalidInputError of string;;
exception SyntaxError of string;;
exception UnboundError of string;;

(* Language Types *)
type langTerm =
	| TmVar of string
	| TmSet of string
	| TmStar of string
	| TmGenWords of langTerm * int
	| TmEq of langTerm * langTerm
	| TmPrint of langTerm
	| TmUni of langTerm * langTerm
	| TmInter of langTerm * langTerm
	| TmConcat of langTerm * langTerm

(* Size of the output set *)
let output_num = ref 0;;
let set_output_num n = output_num := n;;

(* Size of star word *)
let star_to_gen_from_langs = ref 0;;
let set_star_to_gen_from_langs n = if(!star_to_gen_from_langs < n) then star_to_gen_from_langs := n;;

(* Sorts list and removes duplicates *)
let sort_uniq l =
	let sl = List.sort compare l in
	let rec go l acc = match l with
		| [] -> List.rev acc
		| [x] -> List.rev (x::acc)
		| (x1::x2::xs) ->
			if x1 = x2
			then go (x2::xs) acc
		else go (x2::xs) (x1::acc)
	in go sl [];;

(* Converts ":" -> "" *)
let rec convert_colon_to_empty l = List.map (Str.global_replace (Str.regexp "[':']+") "") l;;

(* Converts "" -> ":" *)
let rec convert_empty_to_colon = function
	| [] -> []
	| (h::t) when h = "" -> ":"::(convert_colon_to_empty t)
    | (h::t) ->  h::(convert_colon_to_empty t);;

(* Converts string to list *)
let string_to_list l =
	if (Str.string_match (Str.regexp "^{[' ' \t]*[a-z:]*[' ' \t]*\\(,[' ' \t]*[a-z:]+[' ' \t]*\\)*}$") l 0) then
		let s = Str.global_replace (Str.regexp "[:]+") ":" l in
		convert_colon_to_empty(Str.split (Str.regexp "[,]+") (Str.global_replace (Str.regexp "['{' '}' '\t' ' ']+") "" s))
	else
		raise (InvalidInputError "Error: All languages should consist of words constructed from the English lowercase alphabet and in the format - {abc, bca, ....}");;

(* Chops down list to the size n *)
let rec limit_output l n = match l with
	  [] -> []
	| h::t -> if (n > 0) then h::(limit_output t (n-1)) else [];;

(* Prints list as a set *)
let print_list l n =
	if(n < 0) then
		raise (InvalidInputError "Error: The number of words to be printed cannot be negative")
	else
		print_string "{";
		print_string (String.concat ", " (convert_empty_to_colon (limit_output l n)));
		print_string "}";
		print_newline();;

(* Union of 2 lists *)
let union l1 l2 = sort_uniq (List.append l1 l2);;

(* Intersection of 2 lists *)
let rec contains x s = match s with
	[ ] -> false
	| h::t -> if (x = h) then true else contains x t;;
let intersection l1 l2 =
	let rec result l1 l2 = match l1 with
	[ ] ->[ ]
	| h::t -> if (contains h l2) then h::(result t l2) else result t l2 in
	sort_uniq (result l1 l2);;

(* Concatenation of 2 lists *)
let concatenation l1 l2 =
	let rec join h lst = match lst with
	  [] -> []
	| h2 :: t2 -> (h ^ h2) :: join h t2 in
		let rec concat lst1 lst2 = match lst1 with
		  [] -> []
		| h :: t -> List.append (join h lst2) (concat t lst2) in
		sort_uniq (concat l1 l2);;

(* Star of an input *)
let star l n =
	let rec duplicate l n acc =
		if n>0 then duplicate l (n-1) acc ^ l else "" in
		duplicate l n "";;
let star_of_a_string l n =
	let rec start l n =
		if n>0 then (star l (n-1)) :: (start l (n-1)) else [] in
		List.sort String.compare (start l n);;
let compare_max_size l  =
	let rec comp l2 n = match l2 with
		[] -> n
		| h :: t -> if String.length h > n then comp t (String.length h) else comp t n in
		comp l 0;;
let get_max_star_from_list word l =
	let max_size = compare_max_size l in
		if max_size > !star_to_gen_from_langs then star_of_a_string word (max_size +1) else star_of_a_string word (!star_to_gen_from_langs);;

(* Generate words of a given length *)
let rec generate l x =
	let rec words lst n =
		if n > 1 then (words (concatenation l lst) (n-1)) else lst in
		words l x ;;
let generate_words_of_length l n =
	let rec f l2 x2 =
		if x2 > 0 then List.append (generate l2 x2) (f l2 (x2-1)) else [] in
		List.filter (fun a -> String.length a = n) (f l n);;

(* Create environment to store variables *)
let var_table = Hashtbl.create 50;;
(* Add variables to environment *)
let assign_var key (value: string list) =
	set_star_to_gen_from_langs (compare_max_size value);
	Hashtbl.add var_table key value;;

(* Evaluate *)
let rec eval e = match e with
	| TmPrint(e) -> print_list (evalSet e) !output_num
	| TmEq(e1, e2) -> assign_var (evalVar e1) (evalSet e2)
	| _ -> raise (SyntaxError "Error: Statements should either print or assign values")

and evalVar e = match e with
	| TmVar(e) -> e
	| _ -> raise (SyntaxError "Error - Variable name expected")

and evalSet e = match e with
	| TmVar(e) -> (try Hashtbl.find var_table e with Not_found -> raise (UnboundError ("Error: Variable " ^ e ^ " unbound.")))
	| TmSet(e) -> string_to_list e
	| TmStar(e) -> star_of_a_string e !star_to_gen_from_langs

	| TmUni (TmStar(e1), e2) -> let x = (evalSet e2) in union (get_max_star_from_list e1 x) x
	| TmUni(e1, e2) -> union (evalSet e1) (evalSet e2)

	| TmInter (TmStar(e1), e2) -> let x = (evalSet e2) in intersection (get_max_star_from_list e1 x) x
	| TmInter (e1, e2) -> intersection (evalSet e1) (evalSet e2)

	| TmConcat (TmStar(e1), e2) -> let x = (evalSet e2) in concatenation (get_max_star_from_list e1 x) x
	| TmConcat (e1, e2) -> concatenation (evalSet e1) (evalSet e2)

	| TmGenWords(e1,e2) -> generate_words_of_length (evalSet e1)  e2
	| _ -> raise (SyntaxError "Error: Input cannot be interpreted as a set");;
