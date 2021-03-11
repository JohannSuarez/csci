/* sample lex file for a language where:
    - keywords are hello and world
*/

/* ---- part 1: declarations ---- */

%{
#include<stdio.h>
#include "y.tab.h"
extern YYSTYPE yylval;
int yywrap();
int yyerror(char* s);
%}


/* ---- part 2: token rules ---- */

%%

"hello"  { return(HELLO); }
"world" { return(WORLD); }

[ \t\f\v] {  }

. { return *yytext; }

%%

/* ---- part 3: code ---- */

int yywrap()
{
   /* cleanup at end of input */
   return(1);
}

int yyerror(char* s)
{
   fprintf(stderr, "%s\n", s);
   return 1;
}
