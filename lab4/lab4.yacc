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
%token<long> INTEGER START STOP COMMA NAME POSTAL SPLIT
%type<long> datafile list entry

 /* ---- part 2: grammar rules ----
  */

%%

datafile: START list STOP
   ;

list: entry
   | entry COMMA list

   ;

 /* ---- This entry is for debugging. To invidually verify the regexes for Integer, Name, Split, and Postal ---- 
entry: INTEGER
   | NAME
   | POSTAL

   ;
 */

 
 /* -- The proper rule for entry: An int, followed by a name, then a split, then finally a postal code  */

 entry: INTEGER NAME SPLIT POSTAL
   ;



 /* ---- No change needs to be implemented below this line ---- */

 /* ---- part 3: supporting programs ---- */

%%

 /* begin parsing */
int main() {
   int res = yyparse();
   /*
      Had to modify this print statement otherwise it kept
      complaining about " warning: too many arguments for format [-Wformat-extra-args] "
   */
   printf("\nCompilation complete: %d \n", res);
   return(res);
}

