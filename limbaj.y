%{
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
extern FILE* yyin;
extern char* yytext;
extern int yylineno;


struct informatiiVariabile{
  		char tipul[1000];
  		char nume[1000];
  		int valoare;
  		int asignat;
  	};

  	struct informatiiVariabile variables[1000];


int nrvar=0;
//int functionCount=0;
//int parametersCount=0;
int eval_result[10];
int eval_nr=0;

 int eval(int expresie)
  	{
  		int aux = expresie;
  		if(aux==expresie)
  		{
  			eval_result[eval_nr]=aux;
  			eval_nr++;
  		}
  	} 



int cautarevar(char namevar[1000])
  	{
  		for(int i=0;i<nrvar;i++)
  			if(strcmp(variables[i].nume,namevar)==0)
  				return i;
  		return -1;
  	}



void declarare(char tip[1000], char namevar[1000])
  	{
  		if(cautarevar(namevar)!=-1)
  		{
  			char buffer[1000];
  			sprintf(buffer,"Variabila '%s' a fost deja declarata.",namevar);
  			yyerror(buffer);
  			exit(0);
  		}

  		strcpy(variables[nrvar].nume, namevar);
 		strcpy(variables[nrvar].tipul,tip);
  		variables[nrvar].asignat=0;
  		nrvar++;


  	}


void assign(char tip[1000],int assignedValue)  //modified
  	{
  		if(strcmp(tip,"intreg")==0)
  		{
  			int aux = (int)assignedValue;

  			if(aux!=assignedValue)
  			{
  				char buffer[1000];
  				sprintf(buffer,"nu se poate asigna o valoare de tipul <int> la tipul <%s>.",tip);
  				yyerror(buffer);
  				exit(0);
  			}

  			variables[nrvar-1].asignat=1;
  			variables[nrvar-1].valoare=aux;
  		}

  		if(strcmp(tip,"intreg")==0)
  		{
  			int aux = (int)assignedValue;

  			variables[nrvar-1].asignat=1;
  			variables[nrvar-1].valoare=aux;
  		}

  	} 
	
	
//asignarea pentru variabilele deja declarate
void assigndec(char * name,int value)  
  	{
  		int poz = cautarevar(name);
  		if(poz!=-1)
  			variables[poz].valoare=value;
  		else
  		{
  			char buffer[500];
  			sprintf(buffer,"Nu se poate asigna o valoare unei variabile nedeclarate.");
  			yyerror(buffer);
  			exit(0);
		}

  	} 
	



%}

%union{

	char* str;
	char* dataType;
	int intval;
}

%token <dataType> TIP
%token <str> ID
%token <intval> NR


%token BGIN END ASSIGN 


%token CLASA
%token INCEPUT_CLASA
%token SFARSIT_CLASA
%token PRIVAT
%token PROTEJAT
%token PUBLIC

%token STRUCTURA
%token INCEPUT_STRUCTURA
%token SFARSIT_STRUCTURA

%token DACA
%token ATUNCI
%token END_ATUNCI
%token ALTFEL
%token END_ALTFEL

%token OPERATOR

%token CAT_TIMP
%token EXECUTA
%token END_EXECUTA

%token PENTRU

%token INCREMENTARE
%token DECREMENTARE

%token ADUN
%token SCAD
%token INMULTIT
%token IMPARTIT

%token OPERATOR_L

%token INCEPUT_FUNCTIE
%token SFARSIT_FUNCTIE

%token SIRCOPY
%token SIRCAT
%token SIRLUNG

%type <intval> valoare
%type <intval> lista_expresii
%type <intval> expresie


%start progr
%%
progr:  declaratii bloc {printf("program corect sintactic\n");}
     ;


declaratii :  declaratie ';'
	   | declaratii declaratie ';'
        | clase 
        | struct
	   ;


declaratie : TIP ID {declarare($1,$2);}
           | TIP ID ASSIGN valoare  {declarare($1, $2); assign($1, $4);}
           | TIP ID '(' lista_param ')'
           | TIP ID '(' ')'
		   | functie
		   | TIP ID '[' NR ']'
		   | TIP ID '[' NR ']' ASSIGN valoare
           
           ;

valoare  : NR {$$ = $1;}
		 ;




clase : clasa
     | clase clasa
     ;


clasa : CLASA ID definitie_clasa
     ;

struct : STRUCTURA ID definitie_struct
     ;
definitie_struct : INCEPUT_STRUCTURA bloc_struct SFARSIT_STRUCTURA
               ;
bloc_struct : declaratie ';' bloc_struct
          |
          ;



definitie_clasa : INCEPUT_CLASA definitie SFARSIT_CLASA
               ;


definitie : bloc_privat 
          | bloc_protejat 
          | bloc_public
          | bloc_privat bloc_protejat
          | bloc_privat bloc_public
          | bloc_protejat bloc_public
          | bloc_privat bloc_protejat bloc_public
          ;


bloc_privat : PRIVAT declaratii
          ;


bloc_protejat : PROTEJAT declaratii
               ;


bloc_public : PUBLIC declaratii
          ;




functie : TIP ID '(' lista_param ')' bloc_functie
          ;

lista_param : param
            | lista_param ','  param 
            ;


            
param : TIP ID
      ; 
     



bloc_functie : INCEPUT_FUNCTIE list SFARSIT_FUNCTIE 
               ;


      
/* bloc */
bloc : BGIN list END  
     ;
     
/* lista instructiuni */
list :  statement ';' 
     | list statement ';'
	 |
     ;

/* instructiune */
statement : ID ASSIGN ID 
         | TIP ID ASSIGN NR {declarare($1, $2); assign($1, $4);}
         | ID ASSIGN NR {assigndec($1,$3);}
         | ID ASSIGN lista_expresii	 
         | ID '(' lista_apel ')'
         | ifthenelse statement
         | cattimp statement
         | exec statement 
         | ptr statement
         | ID SIRCOPY ID
         | ID SIRCAT ID
         | SIRLUNG ID
		 | ifthenelse
         | ID '[' NR ']' ASSIGN valoare
         ;
lista_expresii : expresie ADUN expresie {$$ = $1 + $3; eval($$);}
				| expresie SCAD expresie {$$ = $1 - $3; eval($$);}
				| expresie INMULTIT expresie {$$ = $1 * $3; eval($$);}
				| expresie IMPARTIT expresie {$$ = $1 / $3; eval($$);}
				| expresie
				;
expresie : ID {$$ = $1;}
			| NR {$$ = $1;}
			;
lista_apel : NR
           | lista_apel ',' NR
           ;



ifthenelse : DACA '(' ID OPERATOR ID ')' ATUNCI list END_ATUNCI ALTFEL list END_ALTFEL ';'
          | DACA '(' ID OPERATOR ID ')' ATUNCI list END_ATUNCI ';'
          ;

cattimp : CAT_TIMP '(' ID OPERATOR ID ')' EXECUTA list END_EXECUTA ';'
     ;

exec : EXECUTA list END_EXECUTA CAT_TIMP '(' ID OPERATOR ID ')' ';'  
     ;
ptr : PENTRU '(' ID ASSIGN NR ',' ID OPERATOR ID ',' ID ASSIGN ID ADUN ID ')' EXECUTA list END_EXECUTA ';'
     ;
%%
void yyerror(char * s){
printf("eroare: %s la linia:%d\n",s,yylineno);
}

int main(int argc, char** argv){
yyin=fopen(argv[1],"r");
yyparse();

	for(int i=0;i<eval_nr;i++)
    	printf("%d \n", eval_result[i]);
	
	FILE *f=fopen("symbol_table.txt","w");

	fprintf(f,"variabilele folosite sunt:\n");

    for(int i=0;i<nrvar;i++)
		fprintf(f,"%d. name: %s; type: %s;  value: %d;\n",i+1,variables[i].nume,variables[i].tipul, variables[i].valoare);
    
	if(nrvar==0)
        fprintf(f,"nimic\n");
	
	
return 0;
} 