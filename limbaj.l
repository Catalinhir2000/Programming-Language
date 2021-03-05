%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "limbaj.tab.h"
%}
%option noyywrap
%%
"intreg"|"rational"|"caracter"|"sir"|"lung"|"lung tare"|"dublu"|"dublu lung" {yylval.dataType = strdup(yytext);return TIP;}
">"|"<"|"="|"!!"|">="|"<=" {return OPERATOR;}
"begin_progr" {return BGIN;}
"end_progr" {return END;}

"clasa" {return CLASA;}
"inceput clasa{" {return INCEPUT_CLASA;}
"}sfarsit clasa" {return SFARSIT_CLASA;}

"structura" {return STRUCTURA;}
"inceput structura{" {return INCEPUT_STRUCTURA;}
"}sfarsit structura" {return SFARSIT_STRUCTURA;}

"privat" {return PRIVAT;}
"protejat" {return PROTEJAT;}
"public" {return PUBLIC;}

"daca" {return DACA;}
"atunci{" {return ATUNCI;}
"}end_atunci" {return END_ATUNCI;}
"altfel{" {return ALTFEL;}
"}end_altfel" {return END_ALTFEL;}

"cat_timp" {return CAT_TIMP;}
"executa{" {return EXECUTA;}
"}end_executa" {return END_EXECUTA;}

"pentru" {return PENTRU;}

"incrementare" {return INCREMENTARE;}
"decrementare" {return DECREMENTARE;}


"+" {return ADUN;}
"-" {return SCAD;}
"*" {return INMULTIT;}
"/" {return IMPARTIT;}

"sau"|"si" {return OPERATOR_L;}

"inceput_functie{" {return INCEPUT_FUNCTIE;}
"}sfarsit_functie" {return SFARSIT_FUNCTIE;}

"sircopy" {return SIRCOPY;}
"sircat" {return SIRCAT;}
"sirlung" {return SIRLUNG;}

@[_a-zA-Z][_a-zA-Z0-9]* {yylval.str=strdup(yytext); return ID;}
":=" {return ASSIGN;}
[0-9]+ {yylval.intval=atoi(yytext); return NR;}
[ \t] ;
\n {yylineno++;}
. {return yytext[0];}

 
