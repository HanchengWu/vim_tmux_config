%{
#include <stdio.h>
#include <stdlib.h>
struct reg_or_imm {
   int flag; // 0: reg, 1: imm
   int val; // either a reg or an imm
};

#include "expr.y.h"
void yyerror(char *s);
%}

%option noyywrap

%% // begin tokens

[ \n\t]                   // just ignore it

[rR][0-7]                 { yylval.reg = atoi(yytext+1); return REG; }
[0-9]+                    { yylval.imm = atoi(yytext); /*printf("IMMEDIATE (%s,%d) ",yytext,atoi(yytext));*/ return IMMEDIATE; }

"="                       { /*printf("ASSIGN ");*/ return ASSIGN; }
";"                       { /*printf("SEMI ");*/ return SEMI; }
"("                       { /*printf("LPAREN ");*/ return LPAREN; }       
")"                       { /*printf("RPAREN ");*/ return RPAREN; }
"["                       { /*printf("LBRACKET ");*/ return LBRACKET; }
"]"                       { /*printf("RBRACKET ");*/ return RBRACKET; }

"+"                       { /*printf("PLUS ");*/ return PLUS;}
"-"                       { /*printf("MINUS ");*/ return MINUS; }

"//"[^\n]*                //{ printf("COMMENT "); }

.                         { yyerror("Illegal character!"); yyterminate(); }

%% // end tokens

void yyerror(char *s)
{
  fprintf(stderr,"%d: %s %s\n", yylineno, s, yytext);
}
