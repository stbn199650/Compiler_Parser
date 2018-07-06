%{

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

#define ANSI_COLOR_RED_BROAD 		"\x1b[1;31m"
#define ANSI_COLOR_RESET   	 		"\x1b[0m"

/*Extern variables that communicate with lex*/

extern int yylex();
extern FILE *yyin;
void yyerror(char *);
void print_error(char *s,char *variable);

/*	Symbol table function */

void insert_symbol(char* id, char* type, int type_mode);			
void symbol_assign(int id, double data);							
int  lookup_symbol(char* id);									
void print_error(char *s,char *variable);
void symbol_arith(int id, int arith, double data);

int var_num;			//The number of the symbol
int error_flag;
int type_flag;
int pass_flag;			//has error or not

int if_flag;
int else_flag;
int depth[50];
int origin[50];
int for_num;
FILE *fp;

struct data_block{
	
	int assign_bit;
	int type_num;
	char type_name[10];

	int int_value;
	double float_value;

	char name[30];
}symbol_table[100];

%}

/* Token definition */
%token VAR VOID INT FLOAT LB RB LCB RCB NEWLINE
%token STRING ADD SUB MUL DIV MOD INC DEC
%token MT LT MTE LTE EQ NE 
%token ASGN ADDASGN SUBASGN MULASGN DIVASGN MODASGN
%token AND OR NOT
%token NUMBER FLOATNUM ID SEMI
%token PRINT PRINTLN IF ELSE FOR


/* Type definition : Define the type by %union{} to specify the type of token */
%union {
	int int_val;
	double float_val;
	char str_val[150];
}

/* Type declaration : Use %type to specify the type of token within < > if the token or name of grammar rule will return value($$) */

%type <float_val> factor term expression group 
%type <int_val> dcl stmt assign print new print_func arith 
%type <int_val> equal_expr equality_op relation_op selection compound_stmt assign_arith 
%type <int_val> block_items block_item for_stmt for_cond


%type <int_val> ADDASGN SUBASGN MULASGN DIVASGN MODASGN
%type <int_val> PRINT PRINTLN
%type <int_val> NEWLINE
%type <float_val> val
%type <int_val> NUMBER
%type <float_val> FLOATNUM
%type <int_val> INT
%type <int_val> FLOAT
%type <str_val> ID 
%type <str_val> STRING

%%

/* Define your parser grammar rule and the rule action */
prog
	: 
	| prog stmt 	{	
						error_flag = 0; 
						type_flag  = 0; 
					}
	;
stmt
	: dcl	
	| assign
	| arith
	| print	
	| for_stmt
	| selection
	| compound_stmt
	| new	
	;
new
	: NEWLINE	
	;
dcl
	: VAR ID INT NEWLINE
				{	
					if(lookup_symbol($2)==-1){
						fprintf(fp,"ldc 0\n");
						insert_symbol($2,"int",1);
						symbol_assign(lookup_symbol($2),0);
					}
					else{
						print_error("Redeclare the variable",$2);
					}
				}
	| VAR ID FLOAT NEWLINE
				{	
						if(lookup_symbol($2)==-1){
							fprintf(fp,"ldc 0\n");
							insert_symbol($2,"float",2);
							symbol_assign(lookup_symbol($2),0);
						}
						else 
							print_error("Redeclare the variable",$2);
				}
	| VAR ID INT val
	 			{ 	
						if(lookup_symbol($2)==-1 ){
							insert_symbol($2,"int",1);
							if(error_flag==0){
								if(type_flag==2)
									print_error("Assign float to int",$2);	
								symbol_assign(lookup_symbol($2),$4);
							}
						}							
						else{
							print_error("Redeclare the variable",$2);
						}
				}
	| VAR ID FLOAT val		
				{ 	
						if(lookup_symbol($2)==-1){
							insert_symbol($2,"float",2);
							if(error_flag==0){
								symbol_assign(lookup_symbol($2),$4);
							}
							else{
								symbol_assign(lookup_symbol($2),0);
							}
						}
					  	else{
					  		print_error("Redeclare the variable",$2);
					  	}
				}
	;
val
	: ASGN expression NEWLINE		{$$ = $2;}
	;
assign
	: ID ASGN expression 	
							{	
									int temp = lookup_symbol($1);
									if(temp==-1){
											fprintf(fp,"pop\n");	//discard the value expression return
								  		print_error("Undeclare the variable",$1);
									}
								  	else{
								  		if(error_flag==0)
								  			symbol_assign(temp,$3);
										else	
											symbol_assign(temp,0);
								  	}
							}
	| ID INC	{
						int temp = lookup_symbol($1);
						if(temp==-1){
							print_error("Undeclare the variable",$1);
						}
						else{
							if(symbol_table[temp].type_num==1){	//int
								fprintf(fp,"iload %d\n",temp);	
								fprintf(fp,"ldc 1\n");	
								fprintf(fp,"iadd\n");	
								symbol_assign(temp,++symbol_table[temp].int_value);
							}
							else{	//float
								fprintf(fp,"fload %d\n",temp);	
								fprintf(fp,"ldc 1\n");	
								fprintf(fp,"fadd\n");	
								symbol_assign(temp,++symbol_table[temp].float_value);
							}
						}
				}
	| ID DEC	{
						int temp = lookup_symbol($1);
						if(temp==-1){
							print_error("Undeclare the variable",$1);
						}
						else{
							if(symbol_table[temp].type_num==1){	//int
								fprintf(fp,"iload %d\n",temp);	
								fprintf(fp,"ldc 1\n");	
								fprintf(fp,"isub\n");	
								symbol_assign(temp,--symbol_table[temp].int_value);
							}else{	//float
								fprintf(fp,"fload %d\n",temp);	
								fprintf(fp,"ldc 1\n");	
								fprintf(fp,"fsub\n");	
								symbol_assign(temp,--symbol_table[temp].float_value);
							}
						}
				}
	;
arith
	: ID assign_arith factor	{
									int temp = lookup_symbol($1);
									if(temp==-1)
										print_error("Undeclare the variable",$1);
									else{
										fprintf(fp,"pop\n");
										symbol_arith(temp, $2, $3);
									}
								}
	;
assign_arith
	: ADDASGN	{ $$ = 1;}
	| SUBASGN	{ $$ = 2;}
	| MULASGN	{ $$ = 3;}
	| DIVASGN	{ $$ = 4;}
	| MODASGN	{ $$ = 5;}
	;
compound_stmt
	: LCB block_items RCB
	;
block_items
	: block_item
	| block_items block_item
	;
block_item
	: stmt
	;
for_stmt
	: FOR {for_num++;} for_cond
	;
for_cond
	: LB	{ 
				fprintf(fp,"goto for_%d\n",for_num);
				fprintf(fp,"for_%d:\n",for_num);
			} 
		limit RB compound_stmt
					{ 
						fprintf(fp,"goto for_%d\n",for_num);
	  				 	fprintf(fp,"end_for_%d:\n",for_num);
					}
	| assign SEMI	
				{
					fprintf(fp,"goto for_%d\n",for_num);
					fprintf(fp,"for_%d:\n",for_num);
				} 
		for_test
					{ 
						fprintf(fp,"goto for_%d\n",for_num);
	  				 	fprintf(fp,"end_for_%d:\n",for_num);
					}
		 		
				
	;
limit
	: factor LT factor { fprintf(fp,"if_icmpge end_for_%d\n",for_num);}
	;
for_test
	: ID LT NUMBER SEMI ID INC LCB block_items RCB
					{
						int temp = lookup_symbol($5);
							if(temp==-1){
								print_error("Undeclare the variable",$1);
							}else{
								if(symbol_table[temp].type_num==1){	//int
									fprintf(fp,"iload %d\n",temp);	
									fprintf(fp,"ldc 1\n");	
									fprintf(fp,"iadd\n");	
									symbol_assign(temp,++symbol_table[temp].int_value);
								}
								else{	//float
									fprintf(fp,"fload %d\n",temp);	
									fprintf(fp,"ldc 1\n");	
									fprintf(fp,"fadd\n");	
									symbol_assign(temp,++symbol_table[temp].float_value);
								}
							}
							fprintf(fp,"iload %d\n",temp);
							fprintf(fp,"ldc %d\n",$3);
							fprintf(fp,"if_icmpge end_for_%d\n",for_num);
					}
	;

selection
	: IF {
			if_flag++; else_flag=0;
			depth[if_flag]++;
			origin[if_flag]++;
		}
		LB equal_expr RB stmt if_condition
	  		{
				//printf("if_flag: %d\n",if_flag);
				if(else_flag==0){
					fprintf(fp,"goto origin_%d_%d\n",if_flag,origin[if_flag]);
					fprintf(fp,"label_%d_%d:\n",if_flag,depth[if_flag]);
				}
				fprintf(fp,"goto origin_%d_%d\n",if_flag,origin[if_flag]);
				fprintf(fp,"origin_%d_%d:\n",if_flag,origin[if_flag]);
				
				if_flag--;
				else_flag=0;
			}
					
	;
if_condition
	:
	| else_if_stmts
	;
else_if_stmts
	: else_if_stmt
	| else_if_stmts else_if_stmt 
	;

else_if_stmt
	: ELSE  
			{
				fprintf(fp,"goto origin_%d_%d\n",if_flag,origin[if_flag]);
				fprintf(fp,"label_%d_%d:\n",if_flag,depth[if_flag]);
			}
		com
	;
com
	: IF LB	equal_expr RB stmt
	| compound_stmt
				{
					else_flag = 1;
				}
	;

equal_expr
	: expression equality_op expression
					{
						depth[if_flag]++;						
						if($2==1){
							fprintf(fp,"if_icmpne label_%d_%d\n",if_flag,depth[if_flag]);
							$$ = 1;
						}
						else if($2==2){
							fprintf(fp,"if_icmpeq label_%d_%d\n",if_flag,depth[if_flag]);
							$$ = 2;
						}
					}
	| expression relation_op expression
					{
						depth[if_flag]++;
						if($2==1){	//>
							fprintf(fp,"if_icmple label_%d_%d\n",if_flag,depth[if_flag]);
						}
						else if($2==2){	//<
							fprintf(fp,"if_icmpge label_%d_%d\n",if_flag,depth[if_flag]);
						}
						else if($2==3){	//>=
							fprintf(fp,"if_icmplt label_%d_%d\n",if_flag,depth[if_flag]);
						}
						else if($2==4){	//<=
							fprintf(fp,"if_icmpgt label_%d_%d\n",if_flag,depth[if_flag]);
						}
					}
	;

equality_op
	: EQ	{ $$=1;}
	| NE	{ $$=2;}
	;

relation_op
	: MT	{ $$=1;}
	| LT	{ $$=2;}
	| MTE	{ $$=3;}
	| LTE	{ $$=4;}
	;

print
	: print_func group NEWLINE		
							{
								if(error_flag==0){
									if(type_flag==1){											
										fprintf(fp,	"getstatic java/lang/System/out Ljava/io/PrintStream;\n"
														"swap		;swap the top two items on the stack \n"
														"invokevirtual java/io/PrintStream/println(I)V\n" );
									}
									else if(type_flag==2){
										fprintf(fp,	"getstatic java/lang/System/out Ljava/io/PrintStream;\n"
														"swap		;swap the top two items on the stack \n"
														"invokevirtual java/io/PrintStream/println(F)V\n" );
									}										
								}
								else{
									fprintf(fp,	"getstatic java/lang/System/out Ljava/io/PrintStream;\n"
													"swap		;swap the top two items on the stack \n"
													"invokevirtual java/io/PrintStream/println(I)V\n" );
									}
							}
	| print_func LB STRING RB NEWLINE 	
						{	
								fprintf(fp, "ldc \"%s\"\n",$3);
								fprintf(fp,	"getstatic java/lang/System/out Ljava/io/PrintStream;\n"
												"swap		;swap the top two items on the stack \n"
												"invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n" );
									
						}
	;
print_func
	: PRINT		{ 	$$=0;}
	| PRINTLN	{ 	$$=1;}
	;
expression
	: term					{ 	$$ = $1; }
	| expression ADD term	{ 	//printf("Add   \n"); 
									$$ = $1 + $3;
									if(type_flag==1)
										fprintf(fp,"iadd \n");
									else if (type_flag==2)
										fprintf(fp,"fadd \n");
							}
	| expression SUB term	{ 	//printf("Sub   \n"); 
								$$ = $1 - $3;
								if(type_flag==1)
									fprintf(fp,"isub \n");
								else if (type_flag==2)
									fprintf(fp,"fsub \n");
							}
	;
term
	: factor				{ $$ = $1; }
	| term MUL factor		{ 	//printf("Mul   \n");
									$$ = $1 * $3;
									if(type_flag==1)
										fprintf(fp,"imul \n");
									else if (type_flag==2)
										fprintf(fp,"fmul \n");
							}
	| term DIV factor	{ 	
								if($3==0){
									print_error("DIV by 0",NULL);
									error_flag=1;
									//$$ = $1;
									//fprintf(fp," \n");
									$$ = 0;
								}
								else{
								//	printf("DIV   \n"); 
									$$ = $1 / $3; 
									if(type_flag==1)
										fprintf(fp,"idiv \n");
									else if (type_flag==2)
										fprintf(fp,"fdiv \n");
								}
						}
	| term MOD factor	{ 	
									if($3==0){
										print_error("MOD by 0",NULL);
										error_flag=1;
										$$ = $1;
									}
									else{
										//printf("MOD\n"); 
										if(type_flag == 1){
											fprintf(fp,"irem \n");
											$$ = (int)$1 % (int)$3;
										}
										else if(type_flag == 2){
											print_error("MOD by float",NULL);
											error_flag=1;
											$$ = $1;
										}
									}
						}
	;
factor
	: NUMBER			{ 	
								$$ = $1; 
									fprintf(fp,"ldc %d\n",(int)$1);
									if(type_flag!=2)
										type_flag=1;

									if(type_flag==2)
										fprintf(fp,"i2f\n");
						}
	| FLOATNUM			{		 	
								$$ = $1; 
								if(type_flag==1)
									fprintf(fp,"i2f\n");
								if(type_flag!=2)
									type_flag=2;								
								fprintf(fp,"ldc_w %.5lf\n",$1);
						}
	| group					{ 	$$ = $1;}
	| ID 					{ 	
								int temp = lookup_symbol($1);
									if(temp==-1){
										print_error("Undeclare the variable",$1);
										error_flag=1;
										$$ = 0;
										fprintf(fp,"ldc %d\n",0);
									}
									else{
										if(symbol_table[temp].assign_bit==0){
											error_flag=1;
											print_error("Undeclare the variable",$1);
											$$ = 0;
											fprintf(fp,"ldc %d\n",0);
										}
										else{
											if(symbol_table[temp].type_num==1){
												int temp_value = symbol_table[temp].int_value;
												$$ = temp_value;
												fprintf(fp,"iload %d\n",temp);
												if(type_flag!=2)
													type_flag=1;

												if(type_flag==2)
													fprintf(fp,"i2f\n");
											}
											else if(symbol_table[temp].type_num==2){
												if(type_flag==1)
													fprintf(fp,"i2f\n");
												if(type_flag!=2){
													type_flag=2;
												}
												double temp_value = symbol_table[temp].float_value;
												$$ =  temp_value;
												fprintf(fp,"fload %d\n",temp);
											}
										}
									}
							}
	;
group
	: LB expression RB	{ 	$$ = $2; }
	;

%%

int main(int argc, char** argv)
{
    var_num = 0;
	for_num = 0;
	pass_flag = 1;
	if_flag = 0;
	else_flag = 0;
	for(int i=0;i<50;i++){
		depth[i] = 0;
		origin[i] = 0;
	}
	
	fp = fopen("compiler_hw3.j","w");
	
	fprintf(fp,	".class public main\n"
		     		".super java/lang/Object\n"
					".method public static main([Ljava/lang/String;)V\n"
					"	.limit stack %d\n"
					"	.limit locals %d\n",30,10);

    yyparse();


	fprintf(fp,	"return\n"
	    		".end method\n");
	fclose(fp);
	
	if(pass_flag==1)	//no error
		printf("Generated: %s\n","compiler_hw3.j");
	else{	
		//error exist,clean .j fp
   		if(remove("compiler_hw3.j")==0)
			printf("delete .j file success\n");
		else
			printf("delete .j file failed\n");
	}

    return 0;
}

void yyerror(char *s) {
}

void print_error(char *s,char *variable){
	pass_flag = 0;
    printf(ANSI_COLOR_RED_BROAD "<ERROR> %s" ANSI_COLOR_RESET,s);    
	if(variable != NULL)
    	printf(ANSI_COLOR_RED_BROAD " \"%s\"" ANSI_COLOR_RESET ,variable); 
	printf("\n");
}

void insert_symbol(char* s,char* type,int type_mode){

		//printf("Insert a symbol: %s\n",s);
		symbol_table[var_num].assign_bit = 0;
		strcpy(symbol_table[var_num].name,s);

		strcpy(symbol_table[var_num].type_name,type);
		symbol_table[var_num].type_num = type_mode;

		var_num++;	
}

void symbol_assign(int id, double data){
	//printf("data:%d\n",data);
	int type = symbol_table[id].type_num;
	if(type==1){
		symbol_table[id].assign_bit = 1;
		symbol_table[id].int_value = (int)data;
		fprintf(fp,"istore %d\n",id);
	}
	else if(type==2){
		symbol_table[id].assign_bit = 1;
		symbol_table[id].float_value = data;
		fprintf(fp,"fstore %d\n",id);
	}
}

void symbol_arith(int id, int arith, double data){
	int type = symbol_table[id].type_num;
	if(type==1){
		fprintf(fp,"iload %d\n",id);
		fprintf(fp,"ldc %d\n",(int)data);
		if(arith == 1){
			symbol_table[id].assign_bit = 1;
			symbol_table[id].int_value += (int)data;
			//printf("ADDASGN\n");
			fprintf(fp,"iadd\n");
		}else if(arith == 2){
			symbol_table[id].assign_bit = 1;
			symbol_table[id].int_value -= (int)data;
			//printf("SUBASGN\n");
			fprintf(fp,"isub\n");
		}else if(arith == 3){
			symbol_table[id].assign_bit = 1;
			symbol_table[id].int_value *= (int)data;
			//printf("MULASGN\n");
			fprintf(fp,"imul\n");
		}else if(arith == 4){
			symbol_table[id].assign_bit = 1;
			symbol_table[id].int_value /= (int)data;
			//printf("DIVASGN\n");
			fprintf(fp,"idiv\n");
		}else if(arith == 5){
			symbol_table[id].assign_bit = 1;
			symbol_table[id].int_value %= (int)data;
			//printf("MODASGN\n");
			if(type_flag==2)
				print_error("MOD by float",NULL);
			fprintf(fp,"irem\n");
		}
		fprintf(fp,"istore %d\n",id);
	}
	else if(type==2){
		fprintf(fp,"fload %d\n",id);
		if(arith == 1){
			symbol_table[id].assign_bit = 1;
			symbol_table[id].float_value += data;
			//printf("ADDASGN\n");
			fprintf(fp,"ldc %lf\n",data);
			fprintf(fp,"fadd\n");
			fprintf(fp,"fstore %d\n",id);
		}else if(arith == 2){
			symbol_table[id].assign_bit = 1;
			symbol_table[id].float_value -= data;
			//printf("SUBASGN\n");
			fprintf(fp,"ldc %lf\n",data);
			fprintf(fp,"fsub\n");
			fprintf(fp,"fstore %d\n",id);
		}else if(arith == 3){
			symbol_table[id].assign_bit = 1;
			symbol_table[id].float_value *= data;
			//printf("MULASGN\n");
			fprintf(fp,"ldc %lf\n",data);
			fprintf(fp,"fmul\n");
			fprintf(fp,"fstore %d\n",id);
		}else if(arith == 4){
			symbol_table[id].assign_bit = 1;
			symbol_table[id].float_value /= data;
			//printf("DIVASGN\n");
			fprintf(fp,"ldc %lf\n",data);
			fprintf(fp,"fdiv\n");
			fprintf(fp,"fstore %d\n",id);
		}else if(arith == 5){
			print_error("MOD by float number",NULL);
		}
	}
}

int lookup_symbol(char* sym){
	for(int i=0;i<var_num;i++){
		if(strcmp(symbol_table[i].name,sym)==0)
			return i;
	}
	return -1;
}

