%{
#include <string>
#include <iostream>
#include <map>
#include <vector>

using namespace std;

struct Atributos {
  vector<string> v;
  string bloco;
  string end_bloco;
};

struct Variavel{
  string var_type;
  int linha;
};


#define YYSTYPE Atributos

int yylex();
int retorna( int tk );
string gera_label(string);
string gera_index();
string trim(const string &s, const string caracter);
void yyerror( const char* );
void erro( string );
void declarar_var(string);
void verificar_var(string);
void abrir_escopo();
void fechar_escopo();
void imprimir_codigo( vector<string>);
vector <map<string, Variavel>> escopos;
vector<string> resolve_enderecos( vector<string> entrada );
vector<string> concatena( vector<string>, vector<string> );
vector<string> operator+( vector<string>, vector<string> );
vector<string> operator+( vector<string>, string );
vector<string> operator+( string, vector<string> ) ;
vector<string> tokeniza (string s);
vector<string> funcoes;
int linha = 1;
int coluna = 0;
string var_type = "let";
int num_params = 0;

%}

%token NUM STRING ID IF ELSE FOR ASM
%token IGUAL DIFERENTE MAISMAIS WHILE FUNCTION
%token LET CONST VAR MAISIGUAL COMENTARIO BOOL

%left '+' '-'
%left '*' '/'

%%

ROOT  : {abrir_escopo();}CMDs {fechar_escopo(); imprimir_codigo($2.v + "." + funcoes);}

CMDs  : CMD CMDs {$$.v = $1.v + $2.v;}
      | CMD
      ;

CMD :   VARIAVEL ';'
    |   CMD_IF
    |   CMD_WHILE
    |   CMD_FOR
    |   DECLA_FUNC
    |   E ASM ';' 	{ $$.v = $1.v + $2.v; }
    |   ';'         { $$.v.clear(); } 
    |   '{'{abrir_escopo();} CMDs '}'  {fechar_escopo(); $$.v = $3.v;}
    ;

VARIAVEL    :   LET NOMEVAR      {var_type = "let"; $$.v = $2.v;}
            |   VAR NOMEVAR      {var_type = "var"; $$.v = $2.v;}
            |   CONST NOMEVAR    {var_type = "const"; $$.v = $2.v;}
            |   A     {$$.v = $1.v + "^";}
            ;

NOMEVAR   : ID '=' A OUTRAVAR {declarar_var($1.v[0]); $$.v = $1.v + "&" + $1.v + $3.v + "="+"^" + $4.v ; }
          | ID  OUTRAVAR      {declarar_var($1.v[0]); $$.v = $1.v + "&" + $2.v ;}
          ;

OUTRAVAR : ',' NOMEVAR  { $$.v = $2.v; }
         |  %empty      { $$.v.clear(); }                  
         ;

A   :   ID  '=' A                 {verificar_var($1.v[0]); $$.v = $1.v + $3.v + "=";}
    |   ID LVALUEPROP '=' A       {$$.v = $1.v+ "@" + $2.v + $4.v + "[=]"; }
    |   '(' ID ')' LVALUEPROP '=' A       {$$.v = $2.v+ "@" + $4.v + $6.v + "[=]"; }
    |   ID LVALUEPROP '+' A       {$$.v = $1.v+ "@" + $2.v + "[@]"+ $4.v + "+"; }
    |   ID LVALUEPROP '-' A       {$$.v = $1.v+ "@" + $2.v + "[@]"+ $4.v + "-"; }
    |   ID LVALUEPROP '*' A       {$$.v = $1.v+ "@" + $2.v + "[@]"+ $4.v + "*"; }
    |   ID LVALUEPROP             {$$.v = $1.v+ "@" + $2.v + "[@]";}
    |   E                         
    |   ID MAISIGUAL A            {$$.v = $1.v + $1.v + "@" + $3.v + "+"+ "="; }
    |   ID LVALUEPROP MAISIGUAL A { $$.v = $1.v+ "@" + $2.v + $1.v+ "@" + $2.v + "[@]" + $4.v + "+" + "[=]"; }
    ;
RVALUE  : ID MAISMAIS   {verificar_var($1.v[0]); $$.v = $1.v + "@" + $1.v + $1.v + "@" + "1" + "+" + "=" + "^"; }
        ;

DECLA_FUNC  : FUNCTION {abrir_escopo(); num_params = 0;} ID '(' ARGs_FUNC ')' '{' CMDs '}' {
              fechar_escopo();
              // declarar_var($3.v[0]);
              string labelfunc = gera_label("LABELFUNCAO");
              $$.v = $3.v + "&" + $3.v + "{}" + "=" + "'&funcao'" + labelfunc + "[=]" + "^";
              funcoes = funcoes + (":"+labelfunc) + $5.v +  "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" +$8.v +  "^"+ "undefined" + "@" + "'&retorno'" + "@" + "~";}
            | '(' ID ')' '('ARGs_FUNC2')' {num_params++; $$.v = $5.v + to_string(num_params) +$2.v+ "@" +"$";}
            ;
 

LVALUEPROP    :   '[' A ']' LVALUEPROP  { $$.v = $2.v + "[@]" + $4.v ; }
              |   '.' ID LVALUEPROP     { $$.v = $2.v + "[@]" + $3.v; }
              |   '[' A ']'             { $$.v =  $2.v; }
              |   '.' ID                { $$.v = $2.v; }
              ;
CMD_WHILE : WHILE '(' EBOOL ')' CMD {
                              $$.bloco = gera_label("LBL_LOOP");
                              $$.end_bloco = gera_label("LBL_ENDWHILE");
                              $$.v.clear(); $$.v =  $$.v + (":" +$$.bloco) + $3.v + "!" +  $$.end_bloco + "?" + $5.v + $$.bloco + "#" + (":" +  $$.end_bloco);
                              }
          ;

CMD_FOR : FOR '(' VARIAVEL ';' EBOOL ';' A ')' CMD { 
                              $$.bloco = gera_label("LBL_LOOP");
                              $$.end_bloco = gera_label("LBL_ENDFOR");
                              $$.v = $3.v + (":" +$$.bloco) + $5.v + "!" + $$.end_bloco + "?" + $9.v + $7.v + "^" + $$.bloco + "#" +(":" + $$.end_bloco);}

CMD_IF  : IF '(' EBOOL ')' CMD CMD_ELSE {
                              $$.bloco = gera_label("LBL_THEN");
                              $$.end_bloco = gera_label("LBL_ENDIF");
                              $$.v = $3.v + "!" + $$.bloco+ "?" + $5.v + $$.end_bloco + "#" + (":" + $$.bloco) + $6.v +(":" + $$.end_bloco);
                              } 
        ;
CMD_ELSE  : ELSE  CMD   {$$.v = $2.v; }
         |  %empty      { $$.v.clear(); }                  
          ;

EBOOL :  A '<' A       { $$.v = $1.v  + $3.v + "<";}
      | A '>' A       { $$.v = $1.v  + $3.v + ">";}
      | A IGUAL A     { $$.v = $1.v  + $3.v + "==";}
      | A DIFERENTE A { $$.v = $1.v  + $3.v + "!=";}
      | BOOL
      | A        
      ;

E   :   E '+' E {$$.v = $1.v + $3.v + "+";}
    |   E '-' E {$$.v = $1.v + $3.v + "-";}
    |   E '*' E {$$.v = $1.v + $3.v + "*";}
    |   E '/' E {$$.v = $1.v + $3.v + "/";}
    |   F
    |   RVALUE

    ;

F   :   ID     {$$.v = $1.v + "@";}
    |   NUM     
    |   STRING  
    |   ID  '(' ')' {$$.v = $1.v + "#";}
    |   ID  '(' ARGs ')'    {num_params++; $$.v = $3.v + to_string(num_params) +$1.v+ "@" +"$";}
    |   '(' E ')'   {$$ = $2;}
    |   '-' F   {$$.v.clear(); $$.v = $$.v + "0" + $2.v + $1.v;}
    |   '{''}'  {$$.v.clear(); $$.v = $$.v + "{}";}
    |   '['']'  {$$.v.clear(); $$.v = $$.v + "[]";}
    |   BOOL
    ;

ARGs    :   E ',' ARGs {$$.v = $1.v + $3.v;}
        |   E
        ;

F_FUNC  :   ID     
        |   NUM     
        |   STRING  
        ;

ARGs_FUNC     :  F_FUNC ',' ARGs {num_params++; $$.v = $1.v + "&"+ $1.v + "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" + $3.v;}
              |  F_FUNC {$$.v = $1.v + "&"+ $1.v; }
              ;

ARGs_FUNC2     :  F_FUNC ',' ARGs {num_params++; $$.v = $1.v + "@" + "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" + $3.v;}
              |  F_FUNC {$$.v = $1.v + "@"; }
              ;
  
%%

#include "lex.yy.c"

map<int,string> nome_tokens = {
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
}

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

vector<string> tokeniza (string s){
	vector<string> result;
	string aux;
	int inicio = 0, final = s.find(" ");
  while (final > 0) {
      aux = s.substr(inicio, final - inicio);
      result = result + aux;
      inicio = final + 1;
      final = s.find(" ", inicio);
  }
  aux = s.substr(inicio, final - inicio);
  result = result + aux;
  return result;
}

string trim(const string &s, const string caracter) {
    string str4 = s.substr(s.find_first_not_of(caracter[0]));
    str4 = str4.substr(0,str4.find_last_not_of(caracter[1])+1) ;
    return str4;
}

int main(){
    yyparse();
    cout << endl;
    return 0;
}