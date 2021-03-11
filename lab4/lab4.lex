
 /* ---- part 1: declarations ---- */

 /* part 1a: any character sets we want to identify */
Digit [0-9]

 /* part 1b: the C setup */
%{
#include<stdio.h>
#include "y.tab.h"
extern YYSTYPE yylval;
int yywrap();
int yyerror(char* s);

/* row and col keep track of where we are in the source file */
int col=0;
int row=0;
%}

%%
/* Don't touch anything above this */

 /* ---- part 2: token rules ---- */

 /* integers are 1 or more digits */
 /* Change this rule such that ints cant start with 0 */
{Digit}+   { col+=strlen(yytext); return(INTEGER); }

 /* fixed-string tokens */
"DATASET"    { col+=5; return(START); }
"."      { col+=3; return(STOP); }
","        { col++; return(COMMA); }
 /* Add rules for postal code, name and split */

 /* whitespace to skip (newlines treated seperately) */
[ \t\f\v]  { col++; }

 /* adjust row/column after a newline */
([\n])     { row++; col=0; }

 /* anything else is an error, return it as a token so the yacc can reject it */
.          { char errmsg[] = "Unknown char in input: x";
             errmsg[23] = yytext[0];
             yyerror(errmsg);
             return(yytext[0]); }

%%

 /* ---- part 3: supporting code ---- */

 /* cleanup any loose ends at the end of input */
int yywrap()
{
   return(1);
}

 /* process any error messages generated */
int yyerror(char* s)
{
   fprintf(stderr, "\n***Error (row %d, col %d): %s.\n\n", row, col, s);
   return 1;
}

