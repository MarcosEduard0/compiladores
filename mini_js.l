D	        [0-9]
L	        [A-Za-z_$]
WS          [ \t\r\n]
INT	        {D}+
NUM         {INT}(\.{INT})?([Ee][+\-]?{INT})?
STRING      ([\"](([\"][\"])|(\\\")|[^\n"])*[\"])|([\'](([\'][\'])|(\\\')|[^\n'])*[\'])
ID	    	{L}+({L}|{D})*
BOOL        false|true


%%


"let"		{   return retorna(LET);}
"const"   	{   return retorna(CONST);}
"var"		{   return retorna(VAR);}
"+="        {   return retorna(MAISIGUAL);}
"++"        {   return retorna(MAISMAIS);}
"=="        {   return retorna(IGUAL);}
"!="        {   return retorna(DIFERENTE);}
"if"		{   return retorna(IF);}
"else"		{   return retorna(ELSE);}
"while"		{   return retorna(WHILE);}

"\t"            { coluna += 4; }
" "             { coluna++; }
"\n"	        { linha++; coluna = 1; }
{WS}		    { }
{NUM}           { return retorna( NUM ); }
{BOOL}           { return retorna( BOOL ); }
{STRING}	    { return retorna( STRING ); }
"print"         { return retorna( PRINT ); }
{ID}            { return retorna( ID ); }
.               { return retorna( *yytext ); }


%%
