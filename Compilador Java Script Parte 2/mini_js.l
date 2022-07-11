D	        [0-9]
L	        [A-Za-z_$]
WS          [ \t\r\n]
INT	        {D}+
NUM         {INT}(\.{INT})?([Ee][+\-]?{INT})?
STRING1      ([\"](([\"][\"])|(\\\")|[^\n"])*[\"])|([\'](([\'][\'])|(\\\')|[^\n'])*[\'])
STRING2     ([`]([^`])*[`])
STRING      {STRING1}|{STRING2}
ID	    	{L}+({L}|{D})*
BOOL        true|false



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
"for"		{   return retorna(FOR);}
"function"  {   return retorna(FUNCTION);}

"\t"            { coluna += 4; }
" "             { coluna++; }
"\n"	        { linha++; coluna = 0; }

"asm{".*"}"	{
				string lexema = trim( yytext + 3, "{}"); 
                yylval.v = tokeniza( lexema );
				return ASM;
			}

{WS}		    { }
{NUM}           { return retorna( NUM ); }
{BOOL}           { return retorna( BOOL ); }
{STRING}	    { return retorna( STRING ); }
{ID}            { return retorna( ID ); }
.               { return retorna( *yytext ); }


%%
