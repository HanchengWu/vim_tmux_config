%{
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "llvm-c/Core.h"
#include "llvm-c/BitReader.h"
#include "llvm-c/BitWriter.h"

#include "list.h"
#include "symbol.h"

#include <errno.h>

typedef struct myvalue {
  LLVMValueRef val;
  LLVMValueRef addr;
} Mv;

typedef struct multiblock { 
  LLVMBasicBlockRef bb[2];
} Mb;

int num_errors;

extern int yylex();   /* lexical analyzer generated from lex.l */

int yyerror();
int parser_error(const char*);

void minic_abort();
char *get_filename();
int get_lineno();

int loops_found=0;

extern LLVMModuleRef Module;
extern LLVMContextRef Context;
 LLVMBuilderRef Builder;

LLVMValueRef Function=NULL;
LLVMValueRef BuildFunction(LLVMTypeRef RetType, const char *name, 
			   paramlist_t *params);

%}

/* Data structure for tree nodes*/

%union {
  int inum;
  float fnum;
  char * id;
  LLVMTypeRef  type;
  LLVMValueRef value;
  LLVMBasicBlockRef bb;
  Mb mulb;
  Mv mvalue;
  paramlist_t *params;
}

/* these tokens are simply their corresponding int values, more terminals*/

%token SEMICOLON COMMA COLON
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token ASSIGN PLUS MINUS STAR DIV MOD 
%token LT GT LTE GTE EQ NEQ NOT
%token LOGICAL_AND LOGICAL_OR
%token BITWISE_OR BITWISE_XOR LSHIFT RSHIFT BITWISE_INVERT

%token DOT ARROW AMPERSAND QUESTION_MARK

%token FOR WHILE IF ELSE DO STRUCT SIZEOF RETURN SWITCH
%token BREAK CONTINUE CASE
%token INT VOID FLOAT

/* no meaning, just placeholders */
%token STATIC AUTO EXTERN TYPEDEF CONST VOLATILE ENUM UNION REGISTER
/* NUMBER and ID have values associated with them returned from lex*/

%token <inum> CONSTANT_INTEGER /*data type of NUMBER is num union*/
%token <fnum> CONSTANT_FLOAT /*data type of NUMBER is num union*/
%token <id>  ID

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

/* values created by parser*/

%type <id> declarator
%type <params> param_list param_list_opt
%type <value> expression
%type <value> assignment_expression
%type <value> conditional_expression
%type <value> constant_expression
%type <value> logical_OR_expression
%type <value> logical_AND_expression
%type <value> inclusive_OR_expression
%type <value> exclusive_OR_expression
%type <value> AND_expression
%type <value> equality_expression
%type <value> relational_expression
%type <value> shift_expression
%type <value> additive_expression
%type <value> multiplicative_expression
%type <value> cast_expression
%type <value> unary_expression
%type <value> lhs_expression
%type <value> postfix_expression
%type <mvalue> primary_expression
%type <value> constant
%type <type> type_specifier
%type <value> opt_initializer
%type <value> expr_opt 
/* 
   The grammar used here is largely borrowed from Kernighan and Ritchie's "The C
   Programming Language," 2nd Edition, Prentice Hall, 1988. 

   But, some modifications have been made specifically for MiniC!
 */

%%

/* 
   Beginning of grammar: Rules
*/

translation_unit:	  external_declaration
			| translation_unit external_declaration
;

external_declaration:	  function_definition
{
  /* finish compiling function */
  if(num_errors>100)
    {
      minic_abort();
    }
  else if(num_errors==0)
    {
      
    }
}
                        | declaration 
{ 
  /* nothing to be done here */
}
;

function_definition:	  type_specifier ID LPAREN param_list_opt RPAREN 
{
  symbol_push_scope();
  /* This is a mid-rule action */
  BuildFunction($1,$2,$4);  
} 
                          compound_stmt 
{ 
  /* This is the rule completion */
  LLVMBasicBlockRef BB = LLVMGetInsertBlock(Builder);
  if(!LLVMGetBasicBlockTerminator(BB))
    {
      if($1==LLVMInt32Type())	
	{
	  LLVMBuildRet(Builder,LLVMConstInt(LLVMInt32TypeInContext(Context),
					    0,(LLVMBool)1));
	}
      else if($1==LLVMFloatType()) 
	{
	  LLVMBuildRet(Builder,LLVMConstReal(LLVMFloatType(),0.0));
					    
	}
      else
	{
	  LLVMBuildRetVoid(Builder);
	  
	}
    }

  symbol_pop_scope();
  /* make sure basic block has a terminator (a return statement) */
}
                        | type_specifier STAR ID LPAREN param_list_opt RPAREN 
{
  symbol_push_scope();
  BuildFunction(LLVMPointerType($1,0),$3,$5);
} 
                          compound_stmt 
{ 
  /* This is the rule completion */


  /* make sure basic block has a terminator (a return statement) */

  LLVMBasicBlockRef BB = LLVMGetInsertBlock(Builder);
  if(!LLVMGetBasicBlockTerminator(BB))
    {
      LLVMBuildRet(Builder,LLVMConstPointerNull(LLVMPointerType($1,0)));
    }

  symbol_pop_scope();
}
;

declaration:    type_specifier STAR ID opt_initializer SEMICOLON
{
  if (is_global_scope())
    {
      LLVMValueRef g =  LLVMAddGlobal(Module,LLVMPointerType($1,0),$3);
      if($4!=NULL)	LLVMSetInitializer(g,$4);
      symbol_insert($3,g,0);
    } 
  else
    {
      LLVMValueRef alloca=LLVMBuildAlloca(Builder,LLVMPointerType($1,0),$3);
      if($4!=NULL)	LLVMBuildStore(Builder,$4,alloca);      
      symbol_insert($3,  /* map name to alloca */
		    alloca, /* build alloca */
		    0);  /* not an arg */
    }
} 
              | type_specifier ID opt_initializer SEMICOLON
{
  if (is_global_scope())
    {
      LLVMValueRef g=LLVMAddGlobal(Module,$1,$2);
      // Do some checks... if it's okay:
      if($3!=NULL)	LLVMSetInitializer(g,$3);
      symbol_insert($2,g,0);
    }
  else
    {
      LLVMValueRef alloca=LLVMBuildAlloca(Builder,$1,$2);
      if($3!=NULL)	LLVMBuildStore(Builder,$3,alloca);
      symbol_insert($2,  /* map name to alloca */
		    alloca, /* build alloca */
		    0);  /* not an arg */
    }
} 
;

declaration_list:	   declaration
{

}
                         | declaration_list declaration  
{

}
;


type_specifier:		  INT 
{
  $$ = LLVMInt32Type();
}
|                         FLOAT
{
  $$ = LLVMFloatType();
}
|                         VOID
{
  $$ = LLVMVoidType();
}
;

declarator: ID
{
  $$ = $1;
}
;

opt_initializer: ASSIGN constant_expression	      
{
  $$ = $2;
}
		|ASSIGN AMPERSAND primary_expression
{
  $$ = $3.addr;
}
		| // nothing
{
  // indicate there is none
  $$ = NULL;
}
;

param_list_opt:           
{ 
  $$ = NULL;
}
                        | param_list
{ 
  $$ = $1;
}
;

param_list:	
			  param_list COMMA type_specifier declarator
{
  $$ = push_param($1,$4,$3);
}
			| param_list COMMA type_specifier STAR declarator
{
  $$ = push_param($1,$5,LLVMPointerType($3,0));
}
                        | param_list COMMA type_specifier
{
  $$ = push_param($1,NULL,$3);
}
			|  type_specifier declarator
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, $2, $1);
}
			| type_specifier STAR declarator
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, $3, LLVMPointerType($1,0));
}
                        | type_specifier
{
  /* create a parameter list with this as the first entry */
  $$ = push_param(NULL, NULL, $1);
}
;


statement:		  expr_stmt            
			| compound_stmt        
			| selection_stmt       
			| iteration_stmt       
			| jump_stmt            
                        | break_stmt
                        | continue_stmt
                        | case_stmt
;

expr_stmt:	           SEMICOLON            
{ 

}
			|  expression SEMICOLON       
{ 

}
;

compound_stmt:		  LBRACE declaration_list_opt statement_list_opt RBRACE 
{

}
;

declaration_list_opt:	
{

}
			| declaration_list
{

}
;

statement_list_opt:	
{

}
			| statement_list
{

}
;

statement_list:		statement
{

}
			| statement_list statement
{

}
;

break_stmt:               BREAK SEMICOLON
{
	loop_info_t lpinfo = get_loop();
        if(lpinfo.expr!=NULL && lpinfo.exit!=NULL){
 	    LLVMBasicBlockRef after_break=LLVMAppendBasicBlock(Function,"ab");
            LLVMBuildBr(Builder,lpinfo.exit);
	    LLVMPositionBuilderAtEnd(Builder,after_break);
	}
};

case_stmt:                CASE constant_expression COLON
{
  // BONUS: part of switch implementation
};

continue_stmt:            CONTINUE SEMICOLON
{
	loop_info_t lpinfo = get_loop();
        if(lpinfo.expr!=NULL && lpinfo.exit!=NULL){
 	    LLVMBasicBlockRef after_conti=LLVMAppendBasicBlock(Function,"ac");
            LLVMBuildBr(Builder,lpinfo.expr);
	    LLVMPositionBuilderAtEnd(Builder,after_conti);
	}

};

selection_stmt:		  
		          IF LPAREN expression RPAREN {
  LLVMBasicBlockRef ifthen = LLVMAppendBasicBlock(Function,"if.then");
  LLVMBasicBlockRef ifelse = LLVMAppendBasicBlock(Function,"if.else");
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($3),0,1);
  LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, $3, zero, "cond_evaluation");
  LLVMValueRef br   = LLVMBuildCondBr(Builder, cond, ifthen, ifelse); 
  LLVMPositionBuilderAtEnd(Builder, ifthen);
  $<bb>$ = ifelse;

} statement {

  LLVMBasicBlockRef ifjoin = LLVMAppendBasicBlock(Function,"if.join");
  LLVMBuildBr(Builder, ifjoin);
  LLVMPositionBuilderAtEnd(Builder, $<bb>5);
  $<bb>$ = ifjoin;
} ELSE statement { 

  LLVMBuildBr(Builder, $<bb>7);
  LLVMPositionBuilderAtEnd(Builder, $<bb>7);
}
| 		          SWITCH LPAREN expression RPAREN statement 
{
  // +10 BONUS POINTS for a fully correct implementation
}
;

iteration_stmt:		  WHILE LPAREN { 
  /* set up header basic block
     make it the new insertion point */
  LLVMBasicBlockRef w_cond = LLVMAppendBasicBlock(Function,"while.cond");
  LLVMBuildBr(Builder, w_cond);
  LLVMPositionBuilderAtEnd(Builder, w_cond);
  $<bb>$ = w_cond;

} expression RPAREN { 
  /* set up loop body */
  LLVMBasicBlockRef w_body=LLVMAppendBasicBlock(Function,"while.body");
  /* create new body and exit blocks */
  LLVMBasicBlockRef w_join=LLVMAppendBasicBlock(Function,"while.join");

  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($4),0,1);
  LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, $4, zero, "cond_evaluation");
  LLVMValueRef br   = LLVMBuildCondBr(Builder, cond, w_body, w_join); 
 
  LLVMPositionBuilderAtEnd(Builder, w_body);
  $<bb>$=w_join;
  /* to support nesting: */
  push_loop($<bb>3,NULL,NULL,w_join);
} 
  statement
{
  /* finish loop */
  /*loop_info_t info = get_loop();*/
  LLVMBuildBr(Builder,$<bb>3);
  LLVMPositionBuilderAtEnd(Builder, $<bb>6);
  pop_loop();
}

| FOR LPAREN expr_opt 
{
  LLVMBasicBlockRef f_cond = LLVMAppendBasicBlock(Function, "for.cond");
  LLVMBuildBr(Builder, f_cond);
  LLVMPositionBuilderAtEnd(Builder, f_cond);
  $<bb>$= f_cond;
 } 
SEMICOLON expr_opt 
{
  LLVMBasicBlockRef f_body = LLVMAppendBasicBlock(Function, "for.body");
  LLVMBasicBlockRef f_join = LLVMAppendBasicBlock(Function, "for.join");
  LLVMBasicBlockRef f_post = LLVMAppendBasicBlock(Function, "for.post");

 
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($6),0,1);
  LLVMValueRef cond = LLVMBuildICmp(Builder, LLVMIntNE, $6, zero, "for.cond.evaluation");
  LLVMBuildCondBr(Builder, cond, f_body, f_join); 
 
  LLVMPositionBuilderAtEnd(Builder, f_post);
  $<mulb>$.bb[0]=f_body;
  $<mulb>$.bb[1]=f_join;
  push_loop(f_post,NULL,NULL,f_join);
} 
SEMICOLON expr_opt 
{
  LLVMBuildBr(Builder, $<bb>4);//jump to f_cond

  LLVMBasicBlockRef f_post=LLVMGetInsertBlock(Builder);
  LLVMPositionBuilderAtEnd(Builder, $<mulb>7.bb[0]);//f_body 
  $<bb>$=f_post;
}
RPAREN statement
{

  LLVMBuildBr(Builder, $<bb>10);//jump to f_post
  LLVMPositionBuilderAtEnd(Builder, $<mulb>7.bb[1]);//f_join
  pop_loop();
}
;

expr_opt:		
{ 

}
			| expression
{ 
  $$ = $1;
}
;

jump_stmt:		  RETURN SEMICOLON
{ 
  LLVMBuildRetVoid(Builder);

}
			| RETURN expression SEMICOLON
{
  LLVMBuildRet(Builder,$2);
}
;

expression:               assignment_expression
{ 
  $$=$1;
}
;

assignment_expression:    conditional_expression
{
  $$=$1;
}
                        | lhs_expression ASSIGN assignment_expression
{
  /* Implement */
  LLVMValueRef rop=$3;
  if( LLVMTypeOf($1) != LLVMPointerType(LLVMTypeOf($3),0)  ){//if types match, nothing needs to be done
    	//printf("\nTypes of Assignments do not match\n");
        if( LLVMTypeOf($1)==LLVMPointerType( LLVMInt32Type(),0)  ){ //lhs points to an integer
                //printf("\nleft is integer\n");
		if(LLVMTypeOf($3)==LLVMFloatType()){ //rhs is a float, so cast it to integer
			//printf("\nfloat2int\n");                        
 			rop=LLVMBuildFPToSI(Builder, $3, LLVMInt32Type(), "float2int");
		} else if(LLVMTypeOf($3)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
			//printf("\nfloat*2int\n");                        
 			rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "float*2int");
		} else if(LLVMTypeOf($3)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
			//printf("\nint*2int\n");                        
 			rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "int*2int");
		}

	}else if( LLVMTypeOf($1)==LLVMPointerType( LLVMFloatType(),0) ){//lhs points to an float
                //printf("\nleft is float\n");
		if(LLVMTypeOf($3)==LLVMInt32Type()){ //rhs is an int, so cast it to float 
 		
			//printf("\nint2float\n");                        
			rop=LLVMBuildSIToFP(Builder, $3, LLVMFloatType(), "int2float");
		}
	}else if( LLVMTypeOf($1)== LLVMPointerType( LLVMPointerType(LLVMInt32Type(),0), 0 ) ){//lhs points to an int*
	        //printf("\nleft is int*\n");
		if(LLVMTypeOf($3)==LLVMInt32Type()){ //rhs is an int, so cast it to int* 
 		
			//printf("\nint2int*\n");                        
			rop=LLVMBuildIntToPtr(Builder, $3, LLVMPointerType(LLVMInt32Type(),0), "int2int*");
		}
	
	}else if( LLVMTypeOf($1)==  LLVMPointerType( LLVMPointerType(LLVMFloatType(),0), 0) ){//lhs points to an float*
	        //printf("\nleft is float*\n");
		if(LLVMTypeOf($3)==LLVMInt32Type()){ //rhs is an int, so cast it to float* 
 		
			//printf("\nint2float*\n");                        
			rop=LLVMBuildIntToPtr(Builder, $3, LLVMPointerType(LLVMFloatType(),0), "int2float*");
		}
	
	}
  }
  LLVMBuildStore(Builder, rop, $1);
  $$=$3;
}
;


conditional_expression:   logical_OR_expression
{
  $$=$1;
}
                        | logical_OR_expression QUESTION_MARK expression COLON conditional_expression
{
  /* Implement */
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($1),0,1);
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntEQ, $1, zero, "");
  $$ =LLVMBuildSelect(Builder, icmp, $5, $3, "");

}
;

constant_expression:       conditional_expression
{ $$ = $1; }
;

logical_OR_expression:    logical_AND_expression
{
  $$ = $1;
}
                        | logical_OR_expression LOGICAL_OR logical_AND_expression
{
  /* Implement */
  LLVMValueRef lop=$1,rop=$3;
  //check if lop is integer
  if(LLVMTypeOf($1)!=LLVMInt32Type()){
	  //printf("\nOR:lop is not int\n");                        
	  if(LLVMTypeOf($1)==LLVMFloatType()){ 
		//printf("\nfloat2int\n");                        
		lop=LLVMBuildFPToSI(Builder, $1, LLVMInt32Type(), "OR:float2int");
	  } else if(LLVMTypeOf($1)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		//printf("\nfloat*2int\n");                        
		lop=LLVMBuildPtrToInt(Builder, $1, LLVMInt32Type(), "OR:float*2int");
	  } else if(LLVMTypeOf($1)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		//printf("\nint*2int\n");                        
		lop=LLVMBuildPtrToInt(Builder, $1, LLVMInt32Type(), "OR:int*2int");
  	}
  }
  //check if rop is integer
  if(LLVMTypeOf($3)!=LLVMInt32Type()){
	  //printf("\nOR:rop is not int\n");                        
	  if(LLVMTypeOf($3)==LLVMFloatType()){ 
		//printf("\nfloat2int\n");                        
		rop=LLVMBuildFPToSI(Builder, $3, LLVMInt32Type(), "OR:float2int");
	  } else if(LLVMTypeOf($3)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		//printf("\nfloat*2int\n");                        
		rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "OR:float*2int");
	  } else if(LLVMTypeOf($3)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		//printf("\nint*2int\n");                        
		rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "OR:int*2int");
  	}
  }
  LLVMValueRef res_32 = LLVMBuildOr(Builder, lop, rop, "LOGIC_OR_32");//return the value that is not 0
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf(res_32),0,1);
  LLVMValueRef one  = LLVMConstInt(LLVMTypeOf(res_32),1,1);
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntEQ, res_32, zero, "OR.cmp");

  $$ =LLVMBuildSelect(Builder,icmp,zero,one,"");
};

logical_AND_expression:   inclusive_OR_expression
{
  $$ = $1;
}
                        | logical_AND_expression LOGICAL_AND inclusive_OR_expression
{
  /* Implement */
  LLVMValueRef lop=$1,rop=$3;
  //check if lop is integer
  if(LLVMTypeOf($1)!=LLVMInt32Type()){
	  //printf("\nOR:lop is not int\n");                        
	  if(LLVMTypeOf($1)==LLVMFloatType()){ 
		//printf("\nfloat2int\n");                        
		lop=LLVMBuildFPToSI(Builder, $1, LLVMInt32Type(), "AND:float2int");
	  } else if(LLVMTypeOf($1)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		//printf("\nfloat*2int\n");                        
		lop=LLVMBuildPtrToInt(Builder, $1, LLVMInt32Type(), "AND:float*2int");
	  } else if(LLVMTypeOf($1)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		//printf("\nint*2int\n");                        
		lop=LLVMBuildPtrToInt(Builder, $1, LLVMInt32Type(), "AND:int*2int");
  	}
  }
  //check if rop is integer
  if(LLVMTypeOf($3)!=LLVMInt32Type()){
	  //printf("\nOR:rop is not int\n");                        
	  if(LLVMTypeOf($3)==LLVMFloatType()){ 
		//printf("\nfloat2int\n");                        
		rop=LLVMBuildFPToSI(Builder, $3, LLVMInt32Type(), "AND:float2int");
	  } else if(LLVMTypeOf($3)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		//printf("\nfloat*2int\n");                        
		rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "AND:float*2int");
	  } else if(LLVMTypeOf($3)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		//printf("\nint*2int\n");                        
		rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "AND:int*2int");
  	}
  }

  LLVMValueRef res_32 = LLVMBuildAnd(Builder, lop, rop, "LOGIC_AND_32");
  LLVMValueRef zero = LLVMConstInt(LLVMTypeOf(res_32),0,1);
  LLVMValueRef one  = LLVMConstInt(LLVMTypeOf(res_32),1,1);
  LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntEQ, res_32, zero, "AND.cmp");

  $$ =LLVMBuildSelect(Builder,icmp,zero,one,"");
}
;

inclusive_OR_expression:  exclusive_OR_expression
{
    $$=$1;
}
                        | inclusive_OR_expression BITWISE_OR exclusive_OR_expression
{
  /* Implement */
  LLVMValueRef lop=$1,rop=$3;
  //check if lop is integer
  if(LLVMTypeOf($1)!=LLVMInt32Type()){
	  //printf("\nOR:lop is not int\n");                        
	  if(LLVMTypeOf($1)==LLVMFloatType()){ 
		//printf("\nfloat2int\n");                        
		lop=LLVMBuildFPToSI(Builder, $1, LLVMInt32Type(), "AND:float2int");
	  } else if(LLVMTypeOf($1)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		//printf("\nfloat*2int\n");                        
		lop=LLVMBuildPtrToInt(Builder, $1, LLVMInt32Type(), "AND:float*2int");
	  } else if(LLVMTypeOf($1)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		//printf("\nint*2int\n");                        
		lop=LLVMBuildPtrToInt(Builder, $1, LLVMInt32Type(), "AND:int*2int");
  	}
  }
  //check if rop is integer
  if(LLVMTypeOf($3)!=LLVMInt32Type()){
	  //printf("\nOR:rop is not int\n");                        
	  if(LLVMTypeOf($3)==LLVMFloatType()){ 
		//printf("\nfloat2int\n");                        
		rop=LLVMBuildFPToSI(Builder, $3, LLVMInt32Type(), "AND:float2int");
	  } else if(LLVMTypeOf($3)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		//printf("\nfloat*2int\n");                        
		rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "AND:float*2int");
	  } else if(LLVMTypeOf($3)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		//printf("\nint*2int\n");                        
		rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "AND:int*2int");
  	}
  }

  $$=LLVMBuildBinOp(Builder,LLVMOr,lop,rop,"");
}
;

exclusive_OR_expression:  AND_expression
{
  $$ = $1;
}
                        | exclusive_OR_expression BITWISE_XOR AND_expression
{
  /* Implement */
  LLVMValueRef lop=$1,rop=$3;
  //check if lop is integer
  if(LLVMTypeOf($1)!=LLVMInt32Type()){
	  //printf("\nOR:lop is not int\n");                        
	  if(LLVMTypeOf($1)==LLVMFloatType()){ 
		//printf("\nfloat2int\n");                        
		lop=LLVMBuildFPToSI(Builder, $1, LLVMInt32Type(), "AND:float2int");
	  } else if(LLVMTypeOf($1)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		//printf("\nfloat*2int\n");                        
		lop=LLVMBuildPtrToInt(Builder, $1, LLVMInt32Type(), "AND:float*2int");
	  } else if(LLVMTypeOf($1)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		//printf("\nint*2int\n");                        
		lop=LLVMBuildPtrToInt(Builder, $1, LLVMInt32Type(), "AND:int*2int");
  	}
  }
  //check if rop is integer
  if(LLVMTypeOf($3)!=LLVMInt32Type()){
	  //printf("\nOR:rop is not int\n");                        
	  if(LLVMTypeOf($3)==LLVMFloatType()){ 
		//printf("\nfloat2int\n");                        
		rop=LLVMBuildFPToSI(Builder, $3, LLVMInt32Type(), "AND:float2int");
	  } else if(LLVMTypeOf($3)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		//printf("\nfloat*2int\n");                        
		rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "AND:float*2int");
	  } else if(LLVMTypeOf($3)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		//printf("\nint*2int\n");                        
		rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "AND:int*2int");
  	}
  }

  $$=LLVMBuildBinOp(Builder,LLVMXor,lop,rop,"");
}
;

AND_expression:           equality_expression
{
  $$ = $1;
}
                        | AND_expression AMPERSAND equality_expression
{
  /* Implement */
  LLVMValueRef lop=$1,rop=$3;
  //check if lop is integer
  if(LLVMTypeOf($1)!=LLVMInt32Type()){
	  //printf("\nOR:lop is not int\n");                        
	  if(LLVMTypeOf($1)==LLVMFloatType()){ 
		//printf("\nfloat2int\n");                        
		lop=LLVMBuildFPToSI(Builder, $1, LLVMInt32Type(), "AND:float2int");
	  } else if(LLVMTypeOf($1)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		//printf("\nfloat*2int\n");                        
		lop=LLVMBuildPtrToInt(Builder, $1, LLVMInt32Type(), "AND:float*2int");
	  } else if(LLVMTypeOf($1)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		//printf("\nint*2int\n");                        
		lop=LLVMBuildPtrToInt(Builder, $1, LLVMInt32Type(), "AND:int*2int");
  	}
  }
  //check if rop is integer
  if(LLVMTypeOf($3)!=LLVMInt32Type()){
	  //printf("\nOR:rop is not int\n");                        
	  if(LLVMTypeOf($3)==LLVMFloatType()){ 
		//printf("\nfloat2int\n");                        
		rop=LLVMBuildFPToSI(Builder, $3, LLVMInt32Type(), "AND:float2int");
	  } else if(LLVMTypeOf($3)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		//printf("\nfloat*2int\n");                        
		rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "AND:float*2int");
	  } else if(LLVMTypeOf($3)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		//printf("\nint*2int\n");                        
		rop=LLVMBuildPtrToInt(Builder, $3, LLVMInt32Type(), "AND:int*2int");
  	}
  }

  $$=LLVMBuildBinOp(Builder,LLVMAnd,lop,rop,"");

}
;

equality_expression:      relational_expression
{
  $$ = $1;
}
                        | equality_expression EQ relational_expression
{

  /* Implement: use icmp */
  LLVMValueRef icmp;
  if(LLVMTypeOf($1)==LLVMFloatType() && LLVMTypeOf($3)==LLVMFloatType()){
  	icmp = LLVMBuildFCmp(Builder, LLVMRealOEQ, $1, $3, "EQ.cmp");
  }else if(LLVMTypeOf($1)==LLVMInt32Type() && LLVMTypeOf($3)==LLVMInt32Type()){
  	icmp = LLVMBuildICmp(Builder, LLVMIntEQ, $1, $3, "EQ.cmp");
  }else {
  	LLVMValueRef lop=$1,rop=$3;
	if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMIntegerTypeKind){ 
    		//printf("\nleft is inter\n");
		lop=LLVMBuildSIToFP(Builder, $1, LLVMTypeOf($3), "int2float");
    	} else {
    		//printf("\nright is inter\n");
		rop=LLVMBuildSIToFP(Builder, $3, LLVMTypeOf($1), "int2float");
 	}
  	icmp = LLVMBuildFCmp(Builder, LLVMRealOEQ, $1, $3, "EQ.cmp");
  }
  LLVMValueRef zero = LLVMConstInt(LLVMInt32Type(),0,1);
  LLVMValueRef one  = LLVMConstInt(LLVMInt32Type(),1,1);
  $$ =LLVMBuildSelect(Builder,icmp,one,zero,"");

}
                        | equality_expression NEQ relational_expression
{
  /* Implement: use icmp */
  LLVMValueRef icmp;
  if(LLVMTypeOf($1)==LLVMFloatType() && LLVMTypeOf($3)==LLVMFloatType()){
  	icmp = LLVMBuildFCmp(Builder, LLVMRealONE, $1, $3, "EQ.cmp");
  }else if(LLVMTypeOf($1)==LLVMInt32Type() && LLVMTypeOf($3)==LLVMInt32Type()){
  	icmp = LLVMBuildICmp(Builder, LLVMIntNE, $1, $3, "EQ.cmp");
  }else {
  	LLVMValueRef lop=$1,rop=$3;
	if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMIntegerTypeKind){ 
    		//printf("\nleft is inter\n");
		lop=LLVMBuildSIToFP(Builder, $1, LLVMTypeOf($3), "int2float");
    	} else {
    	//	printf("\nright is inter\n");
		rop=LLVMBuildSIToFP(Builder, $3, LLVMTypeOf($1), "int2float");
 	}
  	icmp = LLVMBuildFCmp(Builder, LLVMRealONE, $1, $3, "EQ.cmp");
  }
  LLVMValueRef zero = LLVMConstInt(LLVMInt32Type(),0,1);
  LLVMValueRef one  = LLVMConstInt(LLVMInt32Type(),1,1);
  $$ =LLVMBuildSelect(Builder,icmp,one,zero,"");


}
;

relational_expression:    shift_expression
{
    $$=$1;
}
                        | relational_expression LT shift_expression
{
  /* Implement: use icmp */
  LLVMValueRef icmp;
  if(LLVMTypeOf($1)==LLVMFloatType() && LLVMTypeOf($3)==LLVMFloatType()){
  	icmp = LLVMBuildFCmp(Builder, LLVMRealOLT, $1, $3, "EQ.cmp");
  }else if(LLVMTypeOf($1)==LLVMInt32Type() && LLVMTypeOf($3)==LLVMInt32Type()){
  	icmp = LLVMBuildICmp(Builder, LLVMIntSLT, $1, $3, "EQ.cmp");
  }else {
  	LLVMValueRef lop=$1,rop=$3;
	if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMIntegerTypeKind){ 
    		//printf("\nleft is inter\n");
		lop=LLVMBuildSIToFP(Builder, $1, LLVMTypeOf($3), "int2float");
    	} else {
    		//printf("\nright is inter\n");
		rop=LLVMBuildSIToFP(Builder, $3, LLVMTypeOf($1), "int2float");
 	}
  	icmp = LLVMBuildFCmp(Builder, LLVMRealOLT, $1, $3, "EQ.cmp");
  }
  LLVMValueRef zero = LLVMConstInt(LLVMInt32Type(),0,1);
  LLVMValueRef one  = LLVMConstInt(LLVMInt32Type(),1,1);
  $$ =LLVMBuildSelect(Builder,icmp,one,zero,"");

}
                        | relational_expression GT shift_expression
{
  /* Implement: use icmp */
  LLVMValueRef icmp;
  if(LLVMTypeOf($1)==LLVMFloatType() && LLVMTypeOf($3)==LLVMFloatType()){
  	icmp = LLVMBuildFCmp(Builder, LLVMRealOGT, $1, $3, "EQ.cmp");
  }else if(LLVMTypeOf($1)==LLVMInt32Type() && LLVMTypeOf($3)==LLVMInt32Type()){
  	icmp = LLVMBuildICmp(Builder, LLVMIntSGT, $1, $3, "EQ.cmp");
  }else {
  	LLVMValueRef lop=$1,rop=$3;
	if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMIntegerTypeKind){ 
    		//printf("\nleft is inter\n");
		lop=LLVMBuildSIToFP(Builder, $1, LLVMTypeOf($3), "int2float");
    	} else {
    		//printf("\nright is inter\n");
		rop=LLVMBuildSIToFP(Builder, $3, LLVMTypeOf($1), "int2float");
 	}
  	icmp = LLVMBuildFCmp(Builder, LLVMRealOGT, $1, $3, "EQ.cmp");
  }
  LLVMValueRef zero = LLVMConstInt(LLVMInt32Type(),0,1);
  LLVMValueRef one  = LLVMConstInt(LLVMInt32Type(),1,1);
  $$ =LLVMBuildSelect(Builder,icmp,one,zero,"");

}
                        | relational_expression LTE shift_expression
{
  /* Implement: use icmp */
  LLVMValueRef icmp;
  if(LLVMTypeOf($1)==LLVMFloatType() && LLVMTypeOf($3)==LLVMFloatType()){
  	icmp = LLVMBuildFCmp(Builder, LLVMRealOLE, $1, $3, "EQ.cmp");
  }else if(LLVMTypeOf($1)==LLVMInt32Type() && LLVMTypeOf($3)==LLVMInt32Type()){
  	icmp = LLVMBuildICmp(Builder, LLVMIntSLE, $1, $3, "EQ.cmp");
  }else {
  	LLVMValueRef lop=$1,rop=$3;
	if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMIntegerTypeKind){ 
    		//printf("\nleft is inter\n");
		lop=LLVMBuildSIToFP(Builder, $1, LLVMTypeOf($3), "int2float");
    	} else {
    		//printf("\nright is inter\n");
		rop=LLVMBuildSIToFP(Builder, $3, LLVMTypeOf($1), "int2float");
 	}
  	icmp = LLVMBuildFCmp(Builder, LLVMRealOLE, $1, $3, "EQ.cmp");
  }
  LLVMValueRef zero = LLVMConstInt(LLVMInt32Type(),0,1);
  LLVMValueRef one  = LLVMConstInt(LLVMInt32Type(),1,1);
  $$ =LLVMBuildSelect(Builder,icmp,one,zero,"");

}
                        | relational_expression GTE shift_expression
{
   /* Implement: use icmp */
  LLVMValueRef icmp;
  if(LLVMTypeOf($1)==LLVMFloatType() && LLVMTypeOf($3)==LLVMFloatType()){
  	icmp = LLVMBuildFCmp(Builder, LLVMRealOGE, $1, $3, "EQ.cmp");
  }else if(LLVMTypeOf($1)==LLVMInt32Type() && LLVMTypeOf($3)==LLVMInt32Type()){
  	icmp = LLVMBuildICmp(Builder, LLVMIntSGE, $1, $3, "EQ.cmp");
  }else {
  	LLVMValueRef lop=$1,rop=$3;
	if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMIntegerTypeKind){ 
    		//printf("\nleft is inter\n");
		lop=LLVMBuildSIToFP(Builder, $1, LLVMTypeOf($3), "int2float");
    	} else {
    		//printf("\nright is inter\n");
		rop=LLVMBuildSIToFP(Builder, $3, LLVMTypeOf($1), "int2float");
 	}
  	icmp = LLVMBuildFCmp(Builder, LLVMRealOGE, $1, $3, "EQ.cmp");
  }
  LLVMValueRef zero = LLVMConstInt(LLVMInt32Type(),0,1);
  LLVMValueRef one  = LLVMConstInt(LLVMInt32Type(),1,1);
  $$ =LLVMBuildSelect(Builder,icmp,one,zero,"");
}
;

shift_expression:         additive_expression
{
    $$=$1;
}
                        | shift_expression LSHIFT additive_expression
{
  /* Implement */
 $$ = LLVMBuildShl(Builder,$1,$3,"");
}
                        | shift_expression RSHIFT additive_expression
{
  /* Implement */

 $$ = LLVMBuildLShr(Builder,$1,$3,"");
}
;

additive_expression:      multiplicative_expression
{
  $$ = $1;
}
                        | additive_expression PLUS multiplicative_expression
{
  /* Implement */
  LLVMValueRef lop=$1,rop=$3;
  
  if (   LLVMGetTypeKind(LLVMTypeOf($1))   !=   LLVMGetTypeKind(LLVMTypeOf($3)) ) {
  	//printf("\nadd two different type values here.\n");

  	if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMIntegerTypeKind){ 
    		//printf("\nleft is inter\n");
		lop=LLVMBuildSIToFP(Builder, $1, LLVMTypeOf($3), "int2float");
  	//if (LLVMGetTypeKind(LLVMTypeOf($3)) == LLVMIntegerTypeKind)
    	} else {
    		//printf("\nright is inter\n");
		rop=LLVMBuildSIToFP(Builder, $3, LLVMTypeOf($1), "int2float");
 	}
  }
  $$=LLVMBuildAdd(Builder, lop, rop, "addition");
}
                        | additive_expression MINUS multiplicative_expression
{
  /* Implement */
  LLVMValueRef lop=$1,rop=$3;
   if (   LLVMGetTypeKind(LLVMTypeOf($1))   !=   LLVMGetTypeKind(LLVMTypeOf($3)) ) {
  	//printf("\nadd two different type values here.\n");

  	if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMIntegerTypeKind){ 
    		//printf("\nleft is inter\n");
		lop=LLVMBuildSIToFP(Builder, $1, LLVMTypeOf($3), "int2float");
  	//if (LLVMGetTypeKind(LLVMTypeOf($3)) == LLVMIntegerTypeKind)
    	} else {
    		//printf("\nright is inter\n");
		rop=LLVMBuildSIToFP(Builder, $3, LLVMTypeOf($1), "int2float");
 	}
  }
 $$=LLVMBuildSub(Builder, lop, rop, "sub");
}
;

multiplicative_expression:  cast_expression
{
  $$ = $1;
}
                        | multiplicative_expression STAR cast_expression
{
  /* Implement */ 
  if(LLVMTypeOf($1)==LLVMFloatType() && LLVMTypeOf($3)==LLVMFloatType()){
  	$$ = LLVMBuildFMul(Builder,$1,$3,"imul"); 
  }else if(LLVMTypeOf($1)==LLVMInt32Type() && LLVMTypeOf($3)==LLVMInt32Type()){
  	$$ = LLVMBuildMul(Builder,$1,$3,"imul"); 
  }else {
  	LLVMValueRef lop=$1,rop=$3;
	if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMIntegerTypeKind){ 
    		//printf("\nleft is inter\n");
		lop=LLVMBuildSIToFP(Builder, $1, LLVMTypeOf($3), "int2float");
    	} else {
    		//printf("\nright is inter\n");
		rop=LLVMBuildSIToFP(Builder, $3, LLVMTypeOf($1), "int2float");
 	}
  	$$ = LLVMBuildFMul(Builder,lop,rop,"imul"); 
  }

}
                        | multiplicative_expression DIV cast_expression
{
   if(LLVMTypeOf($1)==LLVMFloatType() && LLVMTypeOf($3)==LLVMFloatType()){
  	$$ = LLVMBuildFDiv(Builder,$1,$3,"imul"); 
  }else if(LLVMTypeOf($1)==LLVMInt32Type() && LLVMTypeOf($3)==LLVMInt32Type()){
  	$$ = LLVMBuildSDiv(Builder,$1,$3,"sidiv");
  }else {
  	LLVMValueRef lop=$1,rop=$3;
	if (LLVMGetTypeKind(LLVMTypeOf($1)) == LLVMIntegerTypeKind){ 
    		//printf("\nleft is inter\n");
		lop=LLVMBuildSIToFP(Builder, $1, LLVMTypeOf($3), "int2float");
    	} else {
    		//printf("\nright is inter\n");
		rop=LLVMBuildSIToFP(Builder, $3, LLVMTypeOf($1), "int2float");
 	}
  	$$ = LLVMBuildFDiv(Builder,lop,rop,"imul"); 
  }

}
                        | multiplicative_expression MOD cast_expression
{
  /* Implement */
  $$ = LLVMBuildSRem(Builder,$1,$3,"sirem");
}
;

cast_expression:          unary_expression
{ $$ = $1; }
;

lhs_expression:           ID 
{
  int isArg=0;
  LLVMValueRef val = symbol_find($1,&isArg);
  $$ = val;
}
                        | STAR ID
{
  LLVMValueRef val = symbol_find($2,NULL);
  $$ = LLVMBuildLoad(Builder,val,"");
}
;

unary_expression:         postfix_expression
{
  $$ = $1;
}
                        | AMPERSAND primary_expression
{
  /* Implement */
  $$=$2.addr;
}
                        | STAR primary_expression
{
  /* primary_expr.val is an address now*/
  $$ = LLVMBuildLoad(Builder,$2.val,"");
}
                        | MINUS unary_expression
{
  /* Implement */
  if(LLVMTypeOf($2)==LLVMInt32Type())	$$ = LLVMBuildNeg(Builder,$2,"NEG");
  if(LLVMTypeOf($2)==LLVMFloatType())	$$ = LLVMBuildFNeg(Builder,$2,"NEG");
}
                        | PLUS unary_expression
{
  $$ = $2;
}
                        | BITWISE_INVERT unary_expression
{
  /* Implement */
  LLVMValueRef op=$2;
  //check if op is integer
  if(LLVMTypeOf($2)!=LLVMInt32Type()){
	  if(LLVMTypeOf($2)==LLVMFloatType()){ 
		op=LLVMBuildFPToSI(Builder, $2, LLVMInt32Type(), "float2int");
	  } else if(LLVMTypeOf($2)== LLVMPointerType(LLVMFloatType(),0) ){ //rhs is a float *, so cast it to integer
		op=LLVMBuildPtrToInt(Builder, $2, LLVMInt32Type(), "float*2int");
	  } else if(LLVMTypeOf($2)== LLVMPointerType(LLVMInt32Type(),0) ){ //rhs is a int *, so cast it to integer
		op=LLVMBuildPtrToInt(Builder, $2, LLVMInt32Type(), "int*2int");
  	}
  }
  $$ = LLVMBuildNot(Builder, op, "BITWISE_INV");

}
                        | NOT unary_expression
{
  /* Implement */
  if(LLVMTypeOf($2)==LLVMInt32Type()){
  	LLVMValueRef zero = LLVMConstInt(LLVMTypeOf($2),0,1);
  	LLVMValueRef icmp = LLVMBuildICmp(Builder, LLVMIntEQ, $2, zero, "NOT.cmp");
  	$$ = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(), "NOT.extend");
  }
  if(LLVMTypeOf($2)==LLVMFloatType()){
  	LLVMValueRef zero = LLVMConstReal(LLVMFloatType(),0.0);
  	LLVMValueRef icmp = LLVMBuildFCmp(Builder, LLVMRealOEQ, $2, zero, "NOT.cmp");
  	$$ = LLVMBuildZExt(Builder, icmp, LLVMInt32Type(), "NOT.extend");
  }
}
;


postfix_expression:       primary_expression
{
  $$ = $1.val;
}
;

primary_expression:       ID 
{ 
  LLVMValueRef val = symbol_find($1,NULL);
  //return an address or a value depending on the type of the ID
  $$.val = LLVMBuildLoad(Builder,val,"");
  $$.addr = val;//keep the address for amperstand
}
                        | constant
{
  $$.val = $1;
}
                        | LPAREN expression RPAREN
{
  $$.val = $2;
}
;

constant:	          CONSTANT_INTEGER  
{ 
  /* Implement */
  $$ = LLVMConstInt(LLVMInt32Type(),$1,0);
} 
|                         CONSTANT_FLOAT
{
  $$ = LLVMConstReal(LLVMFloatType(),$1);
}
;

%%

LLVMValueRef BuildFunction(LLVMTypeRef RetType, const char *name, 
			   paramlist_t *params)
{
  int i;
  int size = paramlist_size(params);
  LLVMTypeRef *ParamArray = malloc(sizeof(LLVMTypeRef)*size);
  LLVMTypeRef FunType;
  LLVMBasicBlockRef BasicBlock;

  paramlist_t *tmp = params;
  /* Build type for function */
  for(i=size-1; i>=0; i--) 
    {
      ParamArray[i] = tmp->type;
      tmp = next_param(tmp);
    }
  
  FunType = LLVMFunctionType(RetType,ParamArray,size,0);

  Function = LLVMAddFunction(Module,name,FunType);
  
  /* Add a new entry basic block to the function */
  BasicBlock = LLVMAppendBasicBlock(Function,"entry");

  /* Create an instruction builder class */
  Builder = LLVMCreateBuilder();

  /* Insert new instruction at the end of entry block */
  LLVMPositionBuilderAtEnd(Builder,BasicBlock);

  tmp = params;
  for(i=size-1; i>=0; i--)
    {
      LLVMValueRef alloca = LLVMBuildAlloca(Builder,tmp->type,tmp->name);
      LLVMBuildStore(Builder,LLVMGetParam(Function,i),alloca);
      symbol_insert(tmp->name,alloca,0);
      tmp=next_param(tmp);
    }

  return Function;
}

extern int line_num;
extern char *infile[];
static int   infile_cnt=0;
extern FILE * yyin;

int parser_error(const char *msg)
{
  printf("%s (%d): Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int internal_error(const char *msg)
{
  printf("%s (%d): Internal Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int yywrap() {
  static FILE * currentFile = NULL;

  if ( (currentFile != 0) ) {
    fclose(yyin);
  }
  
  if(infile[infile_cnt]==NULL)
    return 1;

  currentFile = fopen(infile[infile_cnt],"r");
  if(currentFile!=NULL)
    yyin = currentFile;
  else
    printf("Could not open file: %s",infile[infile_cnt]);

  infile_cnt++;
  
  return (currentFile)?0:1;
}

int yyerror()
{
  parser_error("Un-resolved syntax error.");
  return 1;
}

char * get_filename()
{
  return infile[infile_cnt-1];
}

int get_lineno()
{
  return line_num;
}


void minic_abort()
{
  parser_error("Too many errors to continue.");
  exit(1);
}
