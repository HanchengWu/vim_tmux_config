%{
#include <stdio.h>
int yylex(void);
void yyerror(const char *);
int regCnt=10;

struct reg_or_imm {
   int flag; // 0: reg, 1: imm
   int val; // either a reg or an imm
};

%}

%token REG IMMEDIATE ASSIGN SEMI LPAREN RPAREN LBRACKET RBRACKET MINUS PLUS

%left PLUS MINUS

%union {
  int reg;
  int imm;
  struct reg_or_imm roi;
}

%type <reg> REG expr
%type <imm> IMMEDIATE

%%

program: REG ASSIGN expr SEMI 
{
  //printf("#REG (%d) ASSIGN expr SEMI\n", $1);
  printf("ADD R%d, R%d, 0",$1, $3);
}
;

expr: 

 IMMEDIATE                 
 {
   //printf("#IMMEDIATE (%d)\n",$1);
   printf("SET R%d, %d\n",regCnt,$1);
   $$ = regCnt++;
 }
| REG
{
  //printf("#REG (%d)\n",$1);
  $$ = $1;
}  
| expr PLUS expr  
{
  printf("# expr (%d) PLUS expr (%d)\n",$1,$3);
  printf("ADD R%d, R%d, R%d\n", regCnt, $1, $3);
  $$ = regCnt++;
}  
| expr MINUS expr 
{
  printf("#expr MINUS expr\n");
  printf("SUB R%d, R%d, R%d\n", regCnt, $1, $3);
  $$ = regCnt++;

}  

| LPAREN expr RPAREN 
{
  //printf("LPAREN expr RPAREN\n");
  $$ = $2;
}  

| MINUS expr 
{
  //printf("MINUS expr\n");
  printf("NOT R%d, R%d\n", regCnt, $2);
  printf("ADD R%d, R%d, 1", regCnt, regCnt);
  $$ = regCnt++;

}  

| LBRACKET expr RBRACKET 
{
  //printf("LBRACKET expr RBRACKET\n");
  printf("LDR R%d, R%d, 1", regCnt, $2);
  $$ = regCnt++;
}  

;


%%
