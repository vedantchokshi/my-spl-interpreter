/* File parser.mly */
%{
	open Lang
%}
%token EOL EOF
%token LPAREN RPAREN LSQUARE RSQUARE
%token <string> VARIABLE SET STAR
%token <int> INT
%token EQUALS
%token PRINT
%token UNION INTERSECTION CONCAT

%start main             /* the entry point */
%type <Lang.langTerm> main

%%
main:
	  expr EOL                { $1 }
	| expr EOL EOF            { $1 }
	| expr EOF                { $1 }
	| EOF                     { raise End_of_file }
;

expr:
	| LPAREN expr RPAREN { $2 }
	| VARIABLE EQUALS expr { TmEq(TmVar($1),$3) }
	| VARIABLE { TmVar $1 }
	| SET LSQUARE INT RSQUARE { TmGenWords(TmSet($1),$3) }
	| SET { TmSet $1 }
	| STAR { TmStar $1 }
	| UNION expr expr { TmUni($2,$3) }
	| INTERSECTION expr expr { TmInter($2,$3) }
	| CONCAT expr expr { TmConcat($2,$3) }
	| PRINT expr { TmPrint ($2) }
;