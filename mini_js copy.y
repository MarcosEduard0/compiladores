%{
#include <string>
#include <iostream>
#include <map>
#include <vector>

using namespace std;

struct Atributos {
  // string v;
  vector<string> v;
  string var_type;
};

struct Variavel
{
  string var_type;
  int linha;
};

vector <map<string, Variavel>> escopos;

#define YYSTYPE Atributos

void erro( string msg );
void print( string st );
string gera_label(string label);
string gera_index();
void yyerror( const char* );
int retorna( int tk );
extern "C" int yylex();
void declarar_var(string nome);
void verificar_var(string nome);
void abrir_escopo();
void fechar_escopo();
void imprimir_codigo( vector<string>);


int linha = 1;
int coluna = 1;
string var_type = "let";

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
%token IGUAL DIFERENTE MAISMAIS WHILE
%token LET CONST VAR MAISIGUAL


%left '+' '-'
%left '*' '/'

%%

ROOT  : {abrir_escopo();}S {fechar_escopo(); imprimir_codigo($2.v); }

S   : CMDs S  {$$.v = $1.v + " "+ $2.v;}
    | %empty  { $$.v.clear(); }
    ;

CMDs  : CMD CMDs {$$.v = $1.v + $2.v;}
      | %empty   { $$.v.clear(); } 
      ;

CMD :    VARIAVEL ';'
    |    RVALUE ';' {$$.v = $1.v + "^";}
    |   '{'{abrir_escopo();} CMDs '}'  {fechar_escopo(); $$.v = $3.v;}
    ;
A   : ID  '=' RVALUE  {$$.v = $1.v + $3.v + "=";}   
    | ID  MAISIGUAL RVALUE  {$$.v = $1.v + $1.v +"@"+ $3.v + "+" + "=";}
    ;

    
VARIAVEL    :   LET NOMEVAR      {var_type = "let"; $$.v = $2.v;}
            |   VAR NOMEVAR      {var_type = "var"; $$.v = $2.v;}
            |   CONST NOMEVAR    {var_type = "const"; $$.v = $2.v;}
            ;

NOMEVAR   : ID '=' RVALUE OUTRAVAR  {declarar_var($1.v[0]); $$.v = $1.v + "&" + $1.v + $3.v + "="+"^" + $4.v ; }
          | ID  OUTRAVAR            {declarar_var($1.v[0]); $$.v = $1.v + "&" + $2.v ;}
          ;

OUTRAVAR : ',' NOMEVAR  {$$.v = $2.v; }
         | ',' NOMEVAR  {$$.v = $2.v ;}
         |  %empty      { $$.v.clear(); }                  
         ;
RVALUE  : E
        | A
        | ID
        ;

E   :   E '+' E {$$.v = $1.v + $3.v + "+";}
    |   E '^' E {$$.v = $1.v + $3.v + "^";}
    |   E '-' E {$$.v = $1.v + $3.v + "-";}
    |   E '*' E {$$.v = $1.v + $3.v + "*";}
    |   E '/' E {$$.v = $1.v + $3.v + "/";}
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

void abrir_escopo(){

  map<string,Variavel> escopo;
  escopos.push_back(escopo);
}

void fechar_escopo(){
  escopos.pop_back();
}

void verificar_var(string nome){

  for(int i = 0; i < escopos.size(); i++)
  {
    map<string,Variavel> escopo = escopos[i];
    if(escopo.count(nome) > 0)
      return;
    
  }
  cout << "Erro: a variável '" << nome << "' não foi declarada." << endl;
  exit(1);
}

void declarar_var(string nome)
{
  // cout << nome<<endl;
  map<string,Variavel> escopo = escopos.back();
  if(escopo.count(nome) > 0 && (escopo[nome].var_type == "let" || escopo[nome].var_type == "var" || escopo[nome].var_type == "const"))
  {
    cout << "Erro: a variável '" << nome << "' já foi declarada na linha " << escopo[nome].linha << "." << endl;
    exit(1);
    
  }
  else{
    Variavel v;
    v.var_type = var_type;
    // cout << "entrei "<< v.var_type << nome << linha <<endl;
    v.linha = linha;
    escopos.back()[nome] = v;
  }
}
void imprimir_codigo(vector<string> v){
  vector<string> codigo = resolve_enderecos(v);
  for(string instrucao: codigo)
      cout << instrucao << " ";
  
  // for (int i = 0; i < codigo.size(); i++){
  //       cout << codigo[i];
  //       if(i+1 < codigo.size())
  //         if (codigo[i+1] != "&" && codigo[i+1] != "@" && codigo[i+1] != "}" && codigo[i+1] != "]"){cout << " ";}
  // }
  cout << '.'; 
}
int main(){
    yyparse();
    cout << endl;
    return 0;
}