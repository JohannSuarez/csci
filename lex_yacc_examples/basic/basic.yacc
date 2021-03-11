 /*
   valid token types are
      HELLO - the keyword "hello"
      WORLD - the keyword "world"

   grammar rules are
      program --> welcome statement
      welcome --> HELLO WORLD
      statement --> IDENTIFER = INTEGER
 */

 /* ---- part 1: declarations ---- */

 /* C setup code: libraries, prototypes, etc */
%{
#include<stdio.h>
int yylex(void);
int yywrap();
int yyerror(char* s);
%}

 /* begin processing the top-level component */
%start program

 /* identify the different value types for components */
%union { int inum; char * str; }

 /* identify the valid token types */
%token HELLO WORLD INTEGER IDENTIFIER

 /* for tokens that have a value, identify their type */
%type<inum> INTEGER statement
%type<str> IDENTIFIER

 /* ---- part 2: grammar rules ----
  *   program --> body
  *   body --> welcome, statement
  *   welcome --> hello, world
  *   statement --> identifier, =, integer
  */

%%

program:
    program body    /* only valid case, program matches with null, leaving the body */
    | program error /* error matches anything invalid */
    |               /* the null case */
    ;

body: welcome statement
   {
       /* body had right form, lookup value resulting from statement, field $2 */
       printf("Program complete and correct, statement value was %d\n", $2);
   };

welcome: HELLO WORLD
    {
        printf("\"hello world\" ok.\n");
    }
    ;

statement: IDENTIFIER '=' INTEGER
    {
       /* access the data values associated with IDENTIFIER (field $1)
        *    and INTEGER (field $3), set a value for the statement, $$ */
       $$ = $3;
       printf("%s = %d ok\n", $1, $3);
    }
    ;

 /* ---- part 3: supporting programs ---- */

%%

 /* begin parsing */
int main() {
   printf("Compilation begins:\n\n");
   int res = yyparse();
   printf("Compilation complete.\n", res);
   return(res);
}
