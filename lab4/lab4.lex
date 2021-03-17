
 /* ---- part 1: declarations ---- */

 /* part 1a: any character sets we want to identify */
Alpha [a-zA-Z]
Digit [0-9]
Nonzero [1-9]

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
{Nonzero}{Digit}*  { col+=strlen(yytext); return(INTEGER); }



 /* fixed-string tokens */
 /* Comment: As per Dave's video lecture, keywords should be checked first before identifiers */
 /* So this is correct */ 
"DATASET"   { col+=5; return(START); }
"."         { col+=3; return(STOP); }
","         { col++; return(COMMA); }


 /* Regex for split */
":"         { col++; return(SPLIT); }


 /* Add regexes for postal code and name */
 /* These should be written AFTER the rules of fixed string tokens and integers, because these can be matched by an earlier rule */

 /* NAME REGEX */
({Alpha})+ { col+=strlen(yytext); return(NAME); }

 /* POSTAL REGEX */
({Alpha}{Digit}{Alpha}{Digit}{Alpha}{Digit}) { col+=strlen(yytext); return(POSTAL); }


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

