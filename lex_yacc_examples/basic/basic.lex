 /* sample lex file for a language where:
    - keywords are hello and world
    - integers are one or more digits
    - identifiers are one or more alphabetic characters
    - the only operator is '=' */

 /* ---- part 1: declarations ---- */

 /* part 1a: any character sets we want to identify */
Alpha [a-zA-Z]
Digit [0-9]

 /* part 1b: the C setup */
%{
#include<stdio.h>
#include "y.tab.h"
extern YYSTYPE yylval;
int yywrap();
int yyerror(char* s);
const int MaxLen = 128;
%}

%%

 /* ---- part 2: token rules ---- */

 /* identify any keywords, and return a token type for them */
"hello"    { return(HELLO); }
"world"    { return(WORLD); }

 /* identify anything described by a regex, */
 /*    the yylval.xxx lets you assign a value of a specific type, */
 /*    where the valid types are identified by the union in the .yacc file */
 /* again return a token type */
({Alpha})+ { yylval.str = strdup(yytext); return(IDENTIFIER); }
({Digit})+ { yylval.inum = atoi(yytext); return(INTEGER); }

 /* identify any special characters, use their ascii value as their token type */
"="        { return('='); }

 /* identify any characters that are just to be skipped, e.g. whitespace */
[ \t\f\v]  {  }

 /* anything else is an error, return it as a token so the yacc rules can reject it */
.          { char errmsg[] = "Unknown char in input: x"; errmsg[23] = yytext[0]; yyerror(errmsg); return(yytext[0]); }



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
   fprintf(stderr, "***Error detected: %s\n   continuing with processing.\n", s);
   return 1;
}
