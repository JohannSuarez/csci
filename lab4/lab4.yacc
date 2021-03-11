 /*
 */

 /* ---- part 1: declarations ---- */

 /* C setup code: libraries, prototypes, etc */
%{
#include<stdio.h>
#include<string.h>
int yylex(void);
int yywrap();
int yyerror(char* s);
extern int row;
extern int col;
%}

 /* begin processing the top-level component */
%start datafile

%union { long info; }

 /* identify the valid token types, all have yylval type long */
 /* ---- For every token type that you're using, you have to put it here ---- */
%token<long> INTEGER START STOP COMMA
%type<long> datafile list entry

 /* ---- part 2: grammar rules ----
  */

%%

datafile: START list STOP
   ;

list: entry
   | entry COMMA list
   ;

 /* ---- You're going to change this integer. (See documentation) ---- */
entry: INTEGER
   ;

 /* ---- No change needs to be implemented below this line ---- */

 /* ---- part 3: supporting programs ---- */

%%

 /* begin parsing */
int main() {
   int res = yyparse();
   printf("\nCompilation complete.\n", res);
   return(res);
}

