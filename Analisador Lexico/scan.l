D	        [0-9]
WS	        [ \t\n]*
L	        [A-Za-z_$]
INT	        {D}+
FLOAT	    {INT}(\.{INT})?([Ee][+\-]?{INT})?
FOR         [Ff][Oo][Rr]
ID	        {L}+({L}|{D})*
IF          [Ii][Ff]
STRING      ([\"](([\"][\"])|(\\\")|[^\n"])*[\"])|([\'](([\'][\'])|(\\\')|[^\n'])*[\'])
STRING2     ([`]([^`])*[`])
COMENTARIO  ([/][*]([*][^/]|([/][^/])|[^/])*[*][/])|([/][/][^\n]*)

%%  
{WS}	        { /* ignora espaço */ }    
{IF}	        { return _IF; }
{FOR}         { return _FOR;}
{INT}	        { return _INT; }
{FLOAT}       { return _FLOAT; }
">="	        { return _MAIG; }
"<="    	    { return _MEIG; }
"=="	        { return _IG; }
"!="	        { return _DIF; }
{STRING}      { return _STRING;}
{STRING2}     { return _STRING2;}
{COMENTARIO}  { return _COMENTARIO; }
{ID}    	    { return _ID; }
.             { return yytext[0]; }
%%