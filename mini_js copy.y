%{
#include <string>
#include <iostream>
#include <map>
#include <vector>

using namespace std;

struct Atributos {
  // string v;
  vector<string> v;
};

#define YYSTYPE Atributos

void erro( string msg );
void print( string st );
string gera_label(string label);
string gera_index();
void yyerror( const char* );
int retorna( int tk );
extern "C" int yylex();

int linha = 1;
int coluna = 1;

vector<string> resolve_enderecos( vector<string> entrada );

vector<string> concatena( vector<string> a, vector<string> b ) {
  a.insert( a.end(), b.begin(), b.end() );
  return a;
}

vector<string> operator+( vector<string> a, vector<string> b ) {
  return concatena( a, b );
}

vector<string> operator+( vector<string> a, string b ) {
  a.push_back( b );
  return a;
}

vector<string> operator+( string a, vector<string> b ) {
  vector<string> c= {};
  c = c + a;
  c = c + b;
  return b;
}

%}

%token NUM STRING ID PRINT IF BOOL ELSE
%token IGUAL DIFERENTE MAISMAIS
%token LET CONST VAR MAISIGUAL


%left '+' '-'
%left '*' '/'

%%

ROOT  : S {
            vector<string> c = resolve_enderecos($1.v);
            for (int i = 0; i < c.size(); i++){
                 cout << c[i];
                 if(i+1 < c.size())
                    if (c[i+1] != "&" && c[i+1] != "@" && c[i+1] != "}" && c[i+1] != "]"){cout << " ";}
            }
            cout << '.'; }

S   : CMDs S  {$$.v = $1.v + " "+ $2.v;}
    | %empty  { $$.v.clear(); }
    ;

CMDs  : CMD CMDs {$$.v = $1.v + $2.v;}
      | %empty   { $$.v.clear(); } 
      ;

CMD :   A   ';'   {$$.v = $1.v + "^";}
    |   P       
    |   VARIAVEL 
    |   CMD_IF
    |   ';'          { $$.v.clear(); }
    |   '{' CMDs '}'  {$$.v = $2.v;}
    ;

VARIAVEL    :   LET NEWVAR      {$$.v = $2.v;}
            |   VAR NEWVAR      {$$.v = $2.v;}
            |   CONST NEWVAR    {$$.v = $2.v;}
            ;

NEWVAR  : ID '=' A OTHERVAR { $$.v = $1.v + "&" + $1.v + $3.v + "="+"^" + $4.v ; }
        | ID  OTHERVAR      { $$.v = $1.v + "&" + $2.v ;}
        ;

OTHERVAR : ',' ID '=' A OTHERVAR  { $$.v = $2.v + "&" + $2.v + $4.v + "="+ "^" + $5.v ; }
         | ',' ID   OTHERVAR      { $$.v = $2.v + "&" + $3.v ;}
         |  %empty                { $$.v.clear(); }                  
         ;

A   :   ID  '=' A                 {$$.v = $1.v + $3.v + "=";}
    // |   ID LVALUEPROP '+' A       {$$.v = $1.v+ "@" + $2.v ; }
    |   ID LVALUEPROP '=' A       {$$.v = $1.v+ "@" + $2.v + $4.v + "[=]"; }
    |   ID LVALUEPROP             {$$.v = $1.v+ "@" + $2.v; }
    |   E                         
    |   ID MAISIGUAL A OTHERVAR   {$$.v = $1.v + $1.v + "@" + $3.v + "+"+ "="; }
    |   ID LVALUEPROP MAISIGUAL A { $$.v = $1.v+ "@" + $2.v + $1.v+ "@" + $2.v + "[@]" + $4.v + "+" + "[=]"; }
    |   RVALUE
    ;

LVALUEPROP    :   '[' A ']' LVALUEPROP  { $$.v = $2.v + "[@]" + $4.v ; }
              |   '.' ID LVALUEPROP     { $$.v = $2.v + "[@]" + $3.v; }
              |   '[' A ']'             { $$.v =  $2.v; }
              |   '.' ID                { $$.v = $2.v; }
              |   '+' A                 {$$.v = "[@]"+ $2.v + $1.v;}
              |   '-' A                 {$$.v = "[@]"+ $2.v + $1.v;}
              ;

CMD_IF  : IF '(' EBOOL ')' CMD CMD_ELSE {
                              string then = gera_label("LBL_THEN");
                              string end_if = gera_label("LBL_ENDIF");
                              // $$.v = $3.v + then + "?" + "#"+ (":" + then)+ $5.v + end_if + (":" + end_if);} 
                              $$.v = $3.v + "!" + then +"?" + $5.v + end_if + "#" + (":" + then) + $6.v + (":" + end_if);
                              } 
        ;

CMD_ELSE  : ELSE  CMD   {$$.v = $2.v; }
          | %empty      { $$.v.clear(); }
          ;

EBOOL : A '<' A       { $$.v = $1.v  + $3.v + "<";}
      | A '>' A       { $$.v = $1.v  + $3.v + ">";}
      | A IGUAL A     { $$.v = $1.v  + $3.v + "==";}
      | A DIFERENTE A { $$.v = $1.v  + $3.v + "!=";}
      | RVALUE        
      | E        
      | BOOL          
      ;

RVALUE  : ID MAISMAIS   {$$.v = $1.v + "@" + $1.v + $1.v + "@" + "1" + "+" + "=" + "^"; }
        ;

P   :   PRINT   E   {$$.v = $2.v + " print #";}
    ;

E   :   E '+' E {$$.v = $1.v + $3.v + "+";}
    |   E '^' E {$$.v = $1.v + $3.v + "^";}
    |   E '-' E {$$.v = $1.v + $3.v + "-";}
    |   E '*' E {$$.v = $1.v + $3.v + "*";}
    |   E '/' E {$$.v = $1.v + $3.v + "/";}
    |   RVALUE
    |   F
    ;

F   :   ID     {$$.v = $1.v + "@";}
    |   NUM     
    |   STRING  
    |   ID  '(' ')' {$$.v = $1.v + "#";}
    |   ID  '(' ARGs ')'    {$$.v = $3.v + $1.v + "#";}
    |   '(' E ')'   {$$ = $2;}
    |   '-' F   {$$.v.clear(); $$.v = $$.v + "0" + $2.v + $1.v;}
    |   F '!'   {$$.v = $1.v + "fat #";}
    |   '{''}'  {$$.v = $1.v + $2.v;}
    |   '['']'  {$$.v = $1.v + $2.v;}
    ;

ARGs    :   E ',' ARGs {$$.v = $1.v + $3.v;}
        |   E
        ;
        
%%

#include "lex.yy.c"

map<int,string> nome_tokens = {
  { PRINT, "print" },
  { STRING, "string" },
  { ID, "nome de identificador" },
  { NUM, "número" }
};

string nome_token( int token ) {
  if( nome_tokens.find( token ) != nome_tokens.end() )
    return nome_tokens[token];
  else {
    string r;
    
    r = token;
    return r;
  }
}

int retorna( int tk ) {  
  yylval.v = {yytext}; 
  coluna += strlen( yytext ); 
  return tk;
}

void yyerror(const char* msg){
    cout << endl << "Erro: " << msg << endl
         << "Perto de :'" << yylval.v[0] << "'" <<endl
         << "Linha: " << linha << " " 
         << "Coluna: "<< coluna<< endl;
    exit(0);
}

void print(string st){
    cout << st << " ";
}

string gera_label(string label){
  static int n = 0;
  return label + to_string(++n) + ":";
}

string gera_index(){
  static int n = 0;
  return to_string(++n)+":";
}

vector<string> resolve_enderecos( vector<string> entrada ) {
  map<string,int> label;
  vector<string> saida;
  for( int i = 0; i < entrada.size(); i++ ) 
    if( entrada[i][0] == ':' ) 
        label[entrada[i].substr(1)] = saida.size();
    else
      saida.push_back( entrada[i] );
  
  for( int i = 0; i < saida.size(); i++ ) 
    if( label.count( saida[i] ) > 0 )
        saida[i] = to_string(label[saida[i]]);
    
  return saida;
}

int main(){
    yyparse();
    cout << endl;
    return 0;
}