(* File lexer.mll *)
{
open Parser
exception Eof
}

rule lang = parse
      [' ' '\t'] { lang lexbuf }
	| ['\n'] { EOL }
	| eof { EOF }
	| "print" { PRINT }
	| "union" { UNION }
    | "inter" { INTERSECTION }
	| "concat" { CONCAT }
	| '=' { EQUALS }
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']+ as var { VARIABLE(var) }
	| ['{']['a'-'z' ',' ':']+['}'] as set { SET(set) }
	| ( ['a'-'z']+ as word ) ['*'] { STAR(word) }
	| ['0'-'9']+ as int { INT(int_of_string int) }
	| '(' { LPAREN }
	| ')' { RPAREN }
	| '[' { LSQUARE }
	| ']' { RSQUARE }
	