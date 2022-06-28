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
void abre_escopo();
void fecha_escopo();

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
%token IGUAL DIFERENTE MAISMAIS
%token LET CONST VAR MAISIGUAL


%left '+' '-'
%left '*' '/'

%%

ROOT  : {abre_escopo();}S {
            vector<string> c = resolve_enderecos($2.v);
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
    |   '{'{abre_escopo();} CMDs '}'  {fecha_escopo(); $$.v = $3.v;}
    ;

VARIAVEL    :   LET NEWVAR      {var_type = "let"; $$.v = $2.v;}
            |   VAR NEWVAR      {var_type = "var"; $$.v = $2.v;}
            |   CONST NEWVAR    {var_type = "const"; $$.v = $2.v;}
            ;

NEWVAR  : ID '=' A OTHERVAR {declarar_var($1.v[0]); $$.v = $1.v + "&" + $1.v + $3.v + "="+"^" + $4.v ; }
        | ID  OTHERVAR      {declarar_var($1.v[0]); $$.v = $1.v + "&" + $2.v ;}
        ;

OTHERVAR : ',' ID '=' A OTHERVAR  {declarar_var($2.v[0]); $$.v = $2.v + "&" + $2.v + $4.v + "="+ "^" + $5.v ; }
         | ',' ID   OTHERVAR      {declarar_var($2.v[0]); $$.v = $2.v + "&" + $3.v ;}
         |  %empty                { $$.v.clear(); }                  
         ;

A   :   ID  '=' A                 {verificar_var($1.v[0]); $$.v = $1.v + $3.v + "=";}
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

RVALUE  : ID MAISMAIS   {verificar_var($1.v[0]); $$.v = $1.v + "@" + $1.v + $1.v + "@" + "1" + "+" + "=" + "^"; }
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

void abre_escopo()
{
  map<string,Variavel> escopo;
  escopos.push_back(escopo);
}

void fecha_escopo()
{
  escopos.pop_back();
}
void verificar_var(string nome)
{
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

int main(){
    yyparse();
    cout << endl;
    return 0;
}