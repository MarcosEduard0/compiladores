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

void erro( string );
void print( string );
string gera_label(string);
string gera_index();
void yyerror( const char* );
int retorna( int tk );
extern "C" int yylex();
void declarar_var(string);
void verificar_var(string);
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

CMD :   A   ';'   {$$.v = $1.v + "^";}
    |   P       
    |   VARIAVEL 
    |   CMD_IF
    |   CMD_WHILE
    |   ';'          { $$.v.clear(); }
    |   '{'{abrir_escopo();} CMDs '}'  {fechar_escopo(); $$.v = $3.v;}
    ;

VARIAVEL    :   LET NOMEVAR      {var_type = "let"; $$.v = $2.v;}
            |   VAR NOMEVAR      {var_type = "var"; $$.v = $2.v;}
            |   CONST NOMEVAR    {var_type = "const"; $$.v = $2.v;}
            ;

NOMEVAR  : ID '=' A OUTRAVAR {declarar_var($1.v[0]); $$.v = $1.v + "&" + $1.v + $3.v + "="+"^" + $4.v ; }
        | ID  OUTRAVAR      {declarar_var($1.v[0]); $$.v = $1.v + "&" + $2.v ;}
        ;

OUTRAVAR : ',' ID '=' A OUTRAVAR  {declarar_var($2.v[0]); $$.v = $2.v + "&" + $2.v + $4.v + "="+ "^" + $5.v ; }
         | ',' ID   OUTRAVAR      {declarar_var($2.v[0]); $$.v = $2.v + "&" + $3.v ;}
         |  %empty                { $$.v.clear(); }                  
         ;

A   :   ID  '=' A                 {verificar_var($1.v[0]); $$.v = $1.v + $3.v + "=";}
    // |   ID LVALUEPROP '+' A       {$$.v = $1.v+ "@" + $2.v ; }
    |   ID LVALUEPROP '=' A       {$$.v = $1.v+ "@" + $2.v + $4.v + "[=]"; }
    |   ID LVALUEPROP '+' A       {$$.v = $1.v+ "@" + $2.v + "[@]"+$4.v + "+"; }
    |   ID LVALUEPROP '-' A       {$$.v = $1.v+ "@" + $2.v +"[@]"+ $4.v + "-"; }
    |   ID LVALUEPROP             {$$.v = $1.v+ "@" + $2.v; }
    |   E                         
    |   ID MAISIGUAL A OUTRAVAR   {$$.v = $1.v + $1.v + "@" + $3.v + "+"+ "="; }
    |   ID LVALUEPROP MAISIGUAL A { $$.v = $1.v+ "@" + $2.v + $1.v+ "@" + $2.v + "[@]" + $4.v + "+" + "[=]"; }
    |   RVALUE
    ;
RVALUE  : ID MAISMAIS   {verificar_var($1.v[0]); $$.v = $1.v + "@" + $1.v + $1.v + "@" + "1" + "+" + "=" + "^"; }
        ;
        
LVALUEPROP    :   '[' A ']' LVALUEPROP  { $$.v = $2.v + "[@]" + $4.v ; }
              |   '.' ID LVALUEPROP     { $$.v = $2.v + "[@]" + $3.v; }
              |   '[' A ']'             { $$.v =  $2.v; }
              |   '.' ID                { $$.v = $2.v; }
              ;
CMD_WHILE : WHILE '(' EBOOL ')' CMD {
                              string loop = gera_label("LBL_LOOP");
                              string end_while = gera_label("LBL_ENDWHILE");
                              $$.v.clear(); $$.v =  $$.v + (":" +loop) + $3.v + "!" + end_while + "?" + $5.v + loop + "#" + (":" + end_while);
}
          ;

CMD_IF  : IF '(' EBOOL ')' CMD CMD_ELSE {
                              string then = gera_label("LBL_THEN");
                              string end_if = gera_label("LBL_ENDIF");
                              // $$.v = $3.v + then + "?" +end_if+ "#"+ (":" + then)+ $5.v + (":"+end_if) + $6.v;
                              $$.v = $3.v + "!" + then + "?" + $5.v + end_if + "#" + (":" + then) + $6.v + (":" + end_if);
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
    |   '{''}'  {$$.v.clear(); $$.v = $$.v + "{}";}
    |   '['']'  {$$.v.clear(); $$.v = $$.v + "[]";}
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