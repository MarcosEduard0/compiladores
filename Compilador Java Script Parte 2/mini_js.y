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
int declarar_var(string);
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
int num_elementos = 0;

%}

%token NUM STRING ID IF ELSE FOR ASM RETURN SETA
%token IGUAL DIFERENTE MAISMAIS WHILE FUNCTION ABRE_PAR_SETA
%token LET CONST VAR MAISIGUAL COMENTARIO BOOL ABRE_CHAVES_ATRIB

%left '+' '-'
%left '*' '/'
%left '%'

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
    |   E ASM ';' 	{ $$.v = $1.v + $2.v + "^"; }
    |   RETURN E  ';'         { $$.v = $2.v + "'&retorno'" + "@" + "~";}
    |   ';'         { $$.v.clear(); } 
    // |   '{'{abrir_escopo();} CMDs '}'  {fechar_escopo(); $$.v.clear(); $$.v = $$.v+ $3.v;}
    |   BLOCO_CMDs
    ;

BLOCO_CMDs  :'{' { abrir_escopo(); } CMDs '}'  { fechar_escopo(); $$.v.clear(); $$.v = $$.v + "<{" + $3.v + "}>";}
            | '{''}' {$$.v.clear();}
            ;

VARIAVEL    :   LET   {var_type = "let";}   NOMEVAR   { $$.v = $3.v;}
            |   VAR   {var_type = "var";}   NOMEVAR   { $$.v = $3.v; }
            |   CONST {var_type = "const";} NOMEVAR   { $$.v = $3.v;}
            |   A     {$$.v = $1.v + "^";}
            ;

NOMEVAR   : ID '=' A OUTRAVAR {
                                // $$.v.clear();   $$.v = $1.v + "&" + $1.v + $3.v + "="+"^" + $4.v ; }
                                $$.v.clear();  if(declarar_var($1.v[0])){$$.v = $1.v + "&";}; $$.v = $$.v + $1.v + $3.v + "="+"^" + $4.v ; }
          // | ID  OUTRAVAR      {declarar_var($1.v[0]); $$.v = $1.v + "&" + $2.v ;}
          | ID  OUTRAVAR      {
                              $$.v.clear(); if(declarar_var($1.v[0])){$$.v = $1.v + "&";}; $$.v = $$.v+ $2.v ;}
          ;

OUTRAVAR : ',' NOMEVAR  { $$.v = $2.v; }
         |  %empty      { $$.v.clear(); }                  
         ;

A   :   ID  '=' A                 {$$.v = $1.v + $3.v + "=";}
    |   F  LVALUEPROP '=' A       {$$.v = $1.v + $2.v + $4.v + "[=]";}
    |   ID LVALUEPROP '=' A       {$$.v = $1.v+ "@" + $2.v + $4.v + "[=]"; }
    // |   '(' ID ')' LVALUEPROP '=' A       {$$.v = $2.v+ "@" + $4.v + $6.v + "[=]"; }
    |   ID LVALUEPROP '+' A       {$$.v = $1.v+ "@" + $2.v + "[@]"+ $4.v + "+"; }
    |   ID LVALUEPROP '-' A       {$$.v = $1.v+ "@" + $2.v + "[@]"+ $4.v + "-"; }
    |   ID LVALUEPROP '*' A       {$$.v = $1.v+ "@" + $2.v + "[@]"+ $4.v + "*"; }
    // |   ID LVALUEPROP             {$$.v = $1.v+ "@" + $2.v + "[@]";}
    |   E                         
    |   ID MAISIGUAL A            {$$.v = $1.v + $1.v + "@" + $3.v + "+"+ "="; }
    |   ID LVALUEPROP MAISIGUAL A { $$.v = $1.v+ "@" + $2.v + $1.v+ "@" + $2.v + "[@]" + $4.v + "+" + "[=]"; }
    ;
RVALUE  : ID MAISMAIS   {verificar_var($1.v[0]); $$.v = $1.v + "@" + $1.v + $1.v + "@" + "1" + "+" + "=" + "^"; }
        | FUNC_ANON     
        | BLOCO_ATRIBUTOS { $$.v.clear(); $$.v = $$.v + "{}"+ $1.v; }
        | '[' VETOR']'      { $$.v.clear(); $$.v = $$.v + "[]"+ $2.v; num_elementos = 0;}
        |   FUNC_SETA
        ;
// VETOR : A ELEMENTO { $$.v.clear(); $$.v = $$.v + to_string(num_elementos) + $1.v + "[<=]"; num_elementos++; $1.v = $$.v; $$.v = $1.v + $2.v;}
//       ;

// ELEMENTO : ',' A ELEMENTO { $$.v.clear(); $$.v = $$.v + to_string(num_elementos) + $2.v + "[<=]"; num_elementos++; $2.v = $$.v; $$.v = $2.v + $3.v;}
//         | %empty                 { $$.v.clear(); }        
//          ;

VETOR : A { $$.v.clear(); $$.v = $$.v + to_string(num_elementos) + $1.v + "[<=]"; num_elementos++; $1.v = $$.v; } ELEMENTO {$$.v = $1.v + $3.v;}
      ;

ELEMENTO : ',' A {$$.v.clear(); $$.v = $$.v + to_string(num_elementos) + $2.v + "[<=]"; num_elementos++; $2.v = $$.v; } ELEMENTO { $$.v = $2.v + $4.v;}
        | %empty                 { $$.v.clear(); }        
         ;
BLOCO_ATRIBUTOS :ABRE_CHAVES_ATRIB ATRIBUTO ATRIBUTOS  '}' { $$.v = $2.v + $3.v; }
                // |'{''}' {$$.v.clear();}
                ;

ATRIBUTOS : ',' ATRIBUTO ATRIBUTOS  { $$.v =  $2.v + $3.v;}
          |  %empty                 { $$.v.clear(); }                  
          ;

ATRIBUTO  : ID ':' A  { $$.v = $1.v + $3.v + "[<=]";}
          ;

FUNC_SETA : ARGS_SETA SETA BODY_SETA {
              fechar_escopo();
              string labelfunc = gera_label("LABELFUNCAO");
              $$.v.clear();
              $$.v = $$.v +  "{}" + "'&funcao'" + labelfunc + "[<=]";
              funcoes = funcoes + (":"+labelfunc) + $1.v + $3.v; 
}

ARGS_SETA : ABRE_PAR_SETA ARGS ')' {$$.v = $2.v;}
          | ID {
            abrir_escopo(); 
            $$.v.clear(); if(declarar_var($1.v[0])){$$.v = $1.v + "&";};
            $$.v = $$.v + $1.v + "arguments" + "@" + "0" + "[@]" + "=" + "^";
          }
          ;
          
BODY_SETA : A { $$.v = $1.v + "'&retorno'" + "@" + "~";}
          | '{' CMDs '}' {$$.v = $2.v + "undefined" + "@" + "'&retorno'" + "@" + "~"; }
          ;

DECLA_FUNC  : FUNCTION {abrir_escopo(); num_params = 0;} ID '(' ARGs_FUNC ')' '{' CMDs '}' {
              fechar_escopo();
              string labelfunc = gera_label("LABELFUNCAO");
              $$.v = $3.v + "&" + $3.v + "{}" + "=" + "'&funcao'" + labelfunc + "[=]" + "^";
              funcoes = funcoes + (":"+labelfunc) + $5.v + $8.v + "undefined" + "@" + "'&retorno'" + "@" + "~"; }
            | '(' ID ')' '('ARGs_FUNC2')' {num_params++; $$.v = $5.v + to_string(num_params) +$2.v+ "@" +"$"+"^";}
            ;

ARGS : ID {num_params++;} ',' ARGS {num_params--; 
                                    $$.v.clear(); if(declarar_var($1.v[0])){$$.v = $1.v + "&";}; 
                                    $$.v = $$.v + $1.v + "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" + $4.v;}
      | ID {  $$.v.clear(); if(declarar_var($1.v[0])){$$.v = $1.v + "&";};
              $$.v = $$.v + $1.v + "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^";}
      |  %empty                 { $$.v.clear(); }                  
      ;


ARGs_FUNC :   ID {num_params++;} ',' ARGs_FUNC {
                      $$.v.clear(); if(declarar_var($1.v[0])){$$.v = $1.v + "&";};
                      num_params--; $$.v = $$.v + $1.v + "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" + $4.v;}
          
          |   ID { $$.v.clear(); if(declarar_var($1.v[0])){$$.v = $1.v + "&";};
                    $$.v = $$.v + $1.v + "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^";}

          // |   ID  {num_params++;}  '=' F ',' ARGs_FUNC {
          //             $$.v.clear(); if(declarar_var($1.v[0])){$$.v = $1.v + "&";};
          //             num_params--; $$.v = $$.v + $1.v + $4.v + "=" + "^"+ "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" + $6.v;}

          |   ID '=' F {
                      $$.v.clear(); if(declarar_var($1.v[0])){$$.v = $1.v + "&";};
                      $$.v = $$.v + $1.v + $3.v + "=" + "^"+ "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" ;}

          |   %empty      { $$.v.clear();}    
          ;

ARGs_FUNC2 :   ID {num_params++;} ',' ARGs_FUNC {
                      $$.v.clear(); if(declarar_var($1.v[0])){$$.v = $1.v + "&";};
                      num_params--; $$.v = $$.v + $1.v + "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" + $4.v;}
          
          |   ID { $$.v.clear(); if(declarar_var($1.v[0])){$$.v = $1.v + "&";};
                    $$.v = $$.v + $1.v + "@";}
          |   %empty      { $$.v.clear();}    
          ;

PARAMS  : A PARAM {$$.v = $1.v + $2.v; num_params++; }
        |   %empty      { $$.v.clear(); }    
        ;

PARAM  : ',' A PARAM {$$.v = $2.v + $3.v; num_params++;}
          |   %empty      { $$.v.clear();  }     
          ;

FUNC_ANON   : FUNCTION {abrir_escopo();num_params = 0;} '(' ARGs_FUNC ')' '{' CMDs '}' {
              fechar_escopo();
              string labelfunc = gera_label("LABELFUNCAO");
              $$.v.clear();
              $$.v = $$.v +  "{}" + "'&funcao'" + labelfunc + "[<=]";
              funcoes = funcoes + (":"+labelfunc) + $4.v + $7.v + "undefined" + "@" + "'&retorno'" + "@" + "~"; 
              
 }

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
                              
                              // $$.v = $3.v + $5.v + "!" + $$.bloco + "?" + (":"+$$.end_bloco) + $9.v + $7.v + "^" + $5.v + $$.end_bloco + "?" + (":"+$$.bloco);}


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
      // | BOOL
      | A        
      ;

E   :   E '+' E {$$.v = $1.v + $3.v + "+";}
    |   E '-' E {$$.v = $1.v + $3.v + "-";}
    |   E '*' E {$$.v = $1.v + $3.v + "*";}
    |   E '/' E {$$.v = $1.v + $3.v + "/";}
    |   E '%' E {$$.v = $1.v + $3.v + "%";}
    |   F
    |   RVALUE
    ;

LVALUE : ID LVALUEPROP { $$.v = $1.v+ "@" + $2.v + "[@]";}
       | ID            {  $$.v = $1.v + "@";}
      //  | ID MAISMAIS   {$$.v = $1.v + "@" + $1.v + $1.v + "@" + "1" + "+" + "="+"^"; }
       ;

F   :   LVALUE
    |   NUM     
    |   STRING  
    |   LVALUE '(' PARAMS ')' { $$.v = $3.v + to_string(num_params) + $1.v + "$"; num_params = 0;}
    |   '(' A ')'  {$$ = $2;}
    |   '-' F   {$$.v.clear(); $$.v = $$.v + "0" + $2.v + $1.v;}
    |   '{''}'  {$$.v.clear(); $$.v = $$.v + "{}";}
    |   '['']'  {$$.v.clear(); $$.v = $$.v + "[]";}
    |   BOOL
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

int declarar_var(string nome)
{
  // cout << nome << " "<< var_type<<endl;
  map<string,Variavel> escopo = escopos.back();
  if(escopo.count(nome) > 0 && escopo[nome].var_type == "let")
  {
    cout << "Erro: a variável '" << nome << "' já foi declarada na linha " << escopo[nome].linha << "." << endl;
    exit(1);
    
  }
  
  for(map<string,Variavel> escopo: escopos){
    // cout << escopo[nome].var_type <<" "<< var_type<< nome<<endl;
      if(escopo.count(nome) > 0 && escopo[nome].var_type == "var" && var_type == "var"){
        return 0;
      }
  }
 
  Variavel v;
  v.var_type = var_type;
  // cout << "entrei "<< v.var_type << nome << linha <<endl;
  v.linha = linha;
  escopos.back()[nome] = v;
  return 1;
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