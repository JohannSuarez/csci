 /*
 */

 /* ---- part 1: declarations ---- */

 /* C setup code: libraries, prototypes, etc */
%{
#include<stdio.h>
#include<string.h>
#include "errtypes.h"
int yylex(void);
int yywrap();
int yyerror(char* s);
extern int row;
extern int col;
%}

 /* begin processing the top-level component */
%start script

%union { struct nodeinfo { char content[256]; long datatype; } info; }

 /* identify the valid token types, all have yylval type long */
%token<struct nodeinfo> INTEGER VARIABLE STRING ASSIGN PLUS SEMI FLOAT
%type<struct nodeinfo> statement expression value script statements

 /* ---- part 2: grammar rules ----
  */

%%

script: statements
   ;

statements: statement
   | statement statements
   ;

statement: VARIABLE ASSIGN expression SEMI 
   {
      $<info.datatype>1 = $<info.datatype>3;


      if($<info.datatype>3 == ERRTYPE) {
         printf("%s is an error type\n", $<info.content>1);
      } else if ( $<info.datatype>3 == INTTYPE) {
         printf("%s is an integer type\n", $<info.content>1);
      } else if ($<info.datatype>3 == FLOATTYPE) {
         printf("%s is a float type\n", $<info.content>1);
      } else {
         printf("%s is a string\n", $<info.content>1);
      }


   }
   ;

expression: value 

   ;



expression: value PLUS expression 
   {


      // If first operand is errortype or the second operand is an errortype, LHS will be errortype.
      if ($<info.datatype>1 == ERRTYPE || $<info.datatype>3 == ERRTYPE) {
         $<info.datatype>$ = ERRTYPE;
      }
      // If the first operand's type is the same as the second operand's type, result will be any of the operand's type
      else if ($<info.datatype>1 == $<info.datatype>3 ) {
         $<info.datatype>$ = $<info.datatype>2;
      } 
      // If one of the operand is an int and the other is a float, the result is a float.

      else if ( (($<info.datatype>1 ==  INTTYPE) && ($<info.datatype>3 ==  FLOATTYPE)) || (($<info.datatype>1 ==  FLOATTYPE) && ($<info.datatype>3 ==  INTTYPE))) 
      {
         printf("One of the operands is an int. Converting to float.");
         $<info.datatype>$ = FLOATTYPE;
      }
      else if ( ($<info.datatype>1 ==  STRTYPE) &&  ($<info.datatype>3 ==  STRTYPE)) 
      {
         $<info.datatype>$ = STRTYPE;
      }





   }
   ;


value: INTEGER | STRING | FLOAT 
   {

      $<info.datatype>$ = $<info.datatype>1;

   
   }
   
   ;

 /* ---- part 3: supporting programs ---- */

%%

 /* begin parsing */
int main() {
   int res = yyparse();
   printf("\nCompilation complete.\n", res);
   return(res);
}

