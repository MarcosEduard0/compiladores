%{
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <map>


using namespace std;

int token;
int linha = 1, coluna_atual = 1, coluna_anterior = 0;


void A();
void E();
void E_linha();
void T();
void T_linha();
void F();
void B();
void H();
void V();
void casa( int );
void erro(string);
void print(string);

enum { tk_int = 256, tk_string, tk_float, tk_id,  tk_funcao, tk_print, tk_mdc, tk_max, tk_dtos};

map<int,string> nome_tokens = {
  	{ tk_int, "int" },
  	{ tk_string, "string" },
  	{ tk_float, "float" },
  	{ tk_id, "nome de identificador" },
  	{ tk_print, "print #" },
  	{ tk_mdc, "mdc #" },
  	{ tk_dtos, "dtos #" },
	{ tk_max, "max #" }
};

int tk( int token );

%}
D	        [0-9]
L	        [A-Za-z_$]
INT	        {D}+
FLOAT	    {INT}(\.{INT})?([Ee][+\-]?{INT})?
ID	    	{L}+({L}|{D})*
STRING      ([\"](([\"][\"])|(\\\")|[^\n"])*[\"])|([\'](([\'][\'])|(\\\')|[^\n'])*[\'])
PRINT 		print
MAX			"max("
MDC			"mdc("
DTOS		"dtos("

%%


" "  			{ coluna_anterior = coluna_atual++; }
"\t"			{ coluna_anterior = coluna_atual;  coluna_atual += 2; }
"\n"            { linha++; coluna_anterior = coluna_atual; coluna_atual = 1; }  
{INT}	        { return tk_int; }
{FLOAT}       	{ return tk_float; }
{STRING}      	{ return tk_string;}
{PRINT}			{return tk_print;}
{MAX}			{ return tk_max; }
{DTOS}			{ return tk_dtos; }
{MDC}			{ return tk_mdc; }
{ID}    	    { return tk_id; }
.             	{ return yytext[0]; }

%%

int tk( int token ) {
  coluna_anterior = coluna_atual;
  coluna_atual += strlen( yytext ); 
  return token;
}

int next_token() {
  return yylex();
}

void erro( string msg ) {
  cout << "*** Erro: ***" << endl
       << "Linha: " << linha << ", coluna: " << coluna_anterior << endl
       << msg << endl;
  exit( 1 );
}

string nome_token( int token ) {
  if( nome_tokens.find( token ) != nome_tokens.end() )
    return nome_tokens[token];
  else {
    string r;
    r = (char) token;
    
    return "'" + r + "'";
  }
}

void A() {
 
	if( token ==  tk_id )
	{
		string temp = yytext;
		casa(tk_id);
		print(temp);
		casa('=');
		E();
		print("=");
		casa(';');
	}
	else
	{
		casa(tk_print); 
		E(); 
		print(nome_token( tk_print ));
		casa(';');
	}
	
}

void E() {
  T();
  E_linha();
}

void E_linha() {
  switch( token ) {
    case '+' : casa( '+' ); T(); print( "+"); E_linha(); break;
    case '-' : casa( '-' ); T(); print( "-"); E_linha(); break;
  }
}

void T() {
  B();
  T_linha();
}

void B(){
	switch(token) {
		case '+' :
		{
			casa('+');
			B();
			break;
		}
		case '-' :
		{
			print("0");
			casa('-');
			B();
			print("-");
			break;
		}
		default: F();H();
	}		
}

void H(){
 if(token == '!') {
		casa('!'); F();print("fat #");
	}
	if(token == '^'){
		casa('^');
		B();
		print("^");
	}
}

void T_linha() {
  switch( token ) {
    case '*' : casa( '*' ); B(); print( "*"); T_linha(); break;
    case '/' : casa( '/' ); B(); print( "/"); T_linha(); break;
  }
}

void F() {
	
   switch( token ) {
    case tk_id : 
	{
      string temp = yytext;
      casa( tk_id ); print( temp + " @" ); 
      break;
	} 
    case tk_int : 
	{
      string temp = yytext;
      casa( tk_int ); print( temp );
      break;
	}
	case tk_string : 
	{
      string temp = yytext;
      casa( tk_string ); print( temp );
      break;
	}
	case tk_float : 
	{
      string temp = yytext;
      casa( tk_float ); print( temp );
      break;
	}
	case tk_max:
	{
		string temp = yytext;
      	casa( tk_max ); 
		V();
		casa( ')' );
		print(nome_token( tk_max ));
      	break;
	}
	case tk_dtos:
	{
		string temp = yytext;
      	casa( tk_dtos ); 
		V();
		casa( ')' );
		print(nome_token( tk_dtos ));
      	break;
	}
	case tk_mdc:
	{
		string temp = yytext;
      	casa( tk_mdc ); 
		V();
		casa( ')' );
		print(nome_token( tk_mdc ));
      	break;
	}
    case '(': 
      casa( '(' ); E(); casa( ')' ); break;
  	}
	
}


void V(){
   E();
   if(token==','){ 
	   casa (','); 
	   V();
	}
}

void casa( int esperado ) {
  if( token == esperado )
    token = next_token();
  else {
      cout << "Esperado " << nome_token( esperado ) 
	   << " , encontrado: " << nome_token( token ) << endl;
    exit( 1 );
  }
}

void print( string msg ) {
  cout << msg << " ";
}

void VS() {
  
	if( token == tk_int || token == tk_string || token == tk_id || token == tk_print ) {
		A();
		VS();
  }
}

auto f = &yyunput;

int main() {
  token = next_token();
  VS();
  return 0;
}