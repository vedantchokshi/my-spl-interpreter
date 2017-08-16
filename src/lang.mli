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

val string_to_list: string -> string list
val set_output_num: int -> unit
val set_star_to_gen_from_langs: int -> unit
val assign_var: string -> string list -> unit
val eval: langTerm -> unit
