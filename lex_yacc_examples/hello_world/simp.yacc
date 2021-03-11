/*
   valid token types are
      HELLO - the keyword "hello"
      WORLD - the keyword "world"
   grammar rules are
      program --> HELLO WORLD
*/

/* ---- part 1: declarations ---- */

%{
#include<stdio.h>
int yylex(void);
int yywrap();
int yyerror(char* s);
%}

%start program

%union { int inum; }

/* part 1d: identify token types */
%token HELLO WORLD

/* ---- part 2: grammar rules ---- */

%%

/* program rule(s) */

program: HELLO WORLD
    {
        printf("Processed \"hello world\" successfully.\n");
    }
    ;

/* ---- part 3: supporting programs ---- */

%%

/* begin parsing */
int main() {
   printf("beginning\n");
   int res = yyparse();
   printf("ending, %d\n", res);
   return(res);
}
