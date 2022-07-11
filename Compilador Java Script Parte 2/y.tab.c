/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "mini_js.y"

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


#line 119 "y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    NUM = 258,
    STRING = 259,
    ID = 260,
    IF = 261,
    ELSE = 262,
    FOR = 263,
    ASM = 264,
    IGUAL = 265,
    DIFERENTE = 266,
    MAISMAIS = 267,
    WHILE = 268,
    FUNCTION = 269,
    LET = 270,
    CONST = 271,
    VAR = 272,
    MAISIGUAL = 273,
    COMENTARIO = 274,
    BOOL = 275
  };
#endif
/* Tokens.  */
#define NUM 258
#define STRING 259
#define ID 260
#define IF 261
#define ELSE 262
#define FOR 263
#define ASM 264
#define IGUAL 265
#define DIFERENTE 266
#define MAISMAIS 267
#define WHILE 268
#define FUNCTION 269
#define LET 270
#define CONST 271
#define VAR 272
#define MAISIGUAL 273
#define COMENTARIO 274
#define BOOL 275

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);





#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   256

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  37
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  25
/* YYNRULES -- Number of rules.  */
#define YYNRULES  76
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  153

#define YYUNDEFTOK  2
#define YYMAXUTOK   275


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      30,    31,    23,    21,    29,    22,    34,    24,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    25,
      35,    28,    36,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    32,     2,    33,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    26,     2,    27,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    59,    59,    59,    61,    62,    65,    66,    67,    68,
      69,    70,    71,    72,    72,    75,    76,    77,    78,    81,
      82,    85,    86,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,   100,   103,   103,   109,   113,   114,   115,
     116,   118,   125,   130,   136,   137,   140,   141,   142,   143,
     144,   145,   148,   149,   150,   151,   152,   153,   157,   158,
     159,   160,   161,   162,   163,   164,   165,   166,   169,   170,
     173,   174,   175,   178,   179,   182,   183
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "NUM", "STRING", "ID", "IF", "ELSE",
  "FOR", "ASM", "IGUAL", "DIFERENTE", "MAISMAIS", "WHILE", "FUNCTION",
  "LET", "CONST", "VAR", "MAISIGUAL", "COMENTARIO", "BOOL", "'+'", "'-'",
  "'*'", "'/'", "';'", "'{'", "'}'", "'='", "','", "'('", "')'", "'['",
  "']'", "'.'", "'<'", "'>'", "$accept", "ROOT", "$@1", "CMDs", "CMD",
  "$@2", "VARIAVEL", "NOMEVAR", "OUTRAVAR", "A", "RVALUE", "DECLA_FUNC",
  "$@3", "LVALUEPROP", "CMD_WHILE", "CMD_FOR", "CMD_IF", "CMD_ELSE",
  "EBOOL", "E", "F", "ARGs", "F_FUNC", "ARGs_FUNC", "ARGs_FUNC2", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,    43,    45,    42,    47,    59,   123,   125,    61,    44,
      40,    41,    91,    93,    46,    60,    62
};
# endif

#define YYPACT_NINF (-99)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-51)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -99,     9,   116,   -99,   -99,   -99,    49,   -25,   -17,     3,
     -99,    29,    29,    29,   -99,   149,   -99,    41,   160,    36,
     -99,   116,    12,   -99,   -99,   -99,   -99,   -99,   -99,   134,
     -99,   -99,   180,   180,   146,   180,    65,    64,   191,    85,
     191,    69,    -8,   -99,   -99,   -99,    46,    41,   194,   -99,
     -99,   116,    -4,   209,   -99,   -99,   -99,    53,   194,   194,
     194,   194,   224,   -99,   213,   -99,    -1,   -99,   138,    60,
      47,    10,   180,   180,   180,   180,   180,    -7,     4,    72,
      59,    73,    79,   180,    29,   -99,    83,    11,   -99,   -99,
      24,    24,   -99,   -99,     5,   194,   -99,    10,   -99,   -99,
     -99,   -99,   -99,   -99,   180,   180,   180,   180,   116,   191,
     116,    50,    84,   -99,   -99,    50,    86,    10,   -99,   -99,
     -99,   -99,   -99,   -99,   105,    91,   -99,   -99,   -99,   -99,
      89,    92,   -99,    97,   104,   180,   116,   -99,   180,   194,
     114,   194,   -99,   -99,   -99,   113,   -99,   116,   -99,   116,
     143,   -99,   -99
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       2,     0,     0,     1,    59,    60,    58,     0,     0,     0,
      34,     0,     0,     0,    67,     0,    12,    13,     0,     0,
       3,     5,     0,    18,    57,    10,     8,     9,     7,    30,
      56,    33,     0,     0,     0,     0,     0,    29,     0,     0,
       0,     0,    22,    15,    17,    16,    58,     0,     0,    64,
      65,     0,    58,     0,    66,     4,     6,     0,     0,     0,
       0,     0,     0,    31,    30,    23,    58,    61,    69,     0,
       0,    40,     0,     0,     0,     0,     0,    67,    51,     0,
       0,     0,     0,     0,     0,    20,     0,     0,    63,    11,
      52,    53,    54,    55,    58,     0,    62,    39,    38,    32,
      26,    27,    28,    24,     0,     0,     0,     0,     0,     0,
       0,     0,    22,    21,    14,     0,     0,     0,    68,    37,
      48,    49,    46,    47,    45,     0,    41,    71,    72,    70,
      74,     0,    19,    76,     0,     0,     0,    43,     0,     0,
       0,     0,    36,    25,    44,     0,    73,     0,    75,     0,
       0,    42,    35
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -99,   -99,   -99,   -20,   -98,   -99,   135,    -9,    61,   -10,
     -99,   -99,   -99,     1,   -99,   -99,   -99,   -99,   -38,    -2,
     172,   -89,    74,   -99,   -99
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    20,    21,    51,    22,    43,    85,    23,
      24,    25,    41,   116,    26,    27,    28,   137,    79,    64,
      30,    69,   130,   131,   134
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      29,    55,    81,    44,    45,    38,   118,    37,    31,     3,
     124,    31,   126,    39,   104,   105,    53,    31,   -50,    29,
      83,    84,    63,    65,   -50,    70,    34,    87,    78,    34,
      78,    86,    68,    40,    42,    34,   117,    56,   144,   106,
     107,   115,    35,    35,    36,    36,    53,    60,    61,    29,
     146,   151,   148,   127,   128,   129,    90,    91,    92,    93,
      53,    31,    99,   100,   101,   102,   103,    32,    50,    54,
      71,   125,    98,   112,    82,   113,    34,    33,    89,    34,
      97,    35,    72,    36,   109,    73,    74,    75,     4,     5,
       6,    96,    76,    68,   120,   121,   122,   123,   119,    78,
      11,    12,    13,   108,   110,    14,    29,    15,    29,   111,
     114,    47,   136,    84,   135,    62,   138,    19,   139,     4,
       5,     6,     7,   140,     8,   143,   141,   150,   145,     9,
      10,    11,    12,    13,    29,   142,    14,    68,    15,    68,
     147,    16,    17,    57,   149,    29,    18,    29,    19,     4,
       5,    66,     4,     5,    46,    58,    59,    60,    61,    58,
      59,    60,    61,     4,     5,    52,    14,    95,    15,    14,
     152,    15,    47,   132,    80,    47,    48,    67,    19,    48,
      14,    19,    15,     4,     5,     6,    47,    49,     0,   133,
      48,     0,    19,     0,     4,     5,     6,     4,     5,    66,
      14,     0,    15,     0,     0,     0,    47,     0,     0,     0,
      62,    77,    19,    15,    14,     0,    15,    47,     0,     0,
      47,    62,     0,    19,    48,     0,    19,     4,     5,    94,
      58,    59,    60,    61,    58,    59,    60,    61,     0,     0,
      88,     0,     0,     0,    14,     0,    15,     0,     0,     0,
      47,     0,     0,     0,    48,     0,    19
};

static const yytype_int16 yycheck[] =
{
       2,    21,    40,    12,    13,    30,    95,     6,    12,     0,
     108,    12,   110,    30,    10,    11,    18,    12,    25,    21,
      28,    29,    32,    33,    31,    35,    30,    31,    38,    30,
      40,    51,    34,    30,     5,    30,    31,    25,   136,    35,
      36,    30,    32,    32,    34,    34,    48,    23,    24,    51,
     139,   149,   141,     3,     4,     5,    58,    59,    60,    61,
      62,    12,    72,    73,    74,    75,    76,    18,    27,    33,
       5,   109,    71,    83,     5,    84,    30,    28,    25,    30,
      33,    32,    18,    34,    25,    21,    22,    23,     3,     4,
       5,    31,    28,    95,   104,   105,   106,   107,    97,   109,
      15,    16,    17,    31,    31,    20,   108,    22,   110,    30,
      27,    26,     7,    29,    28,    30,    25,    32,    29,     3,
       4,     5,     6,    31,     8,   135,    29,   147,   138,    13,
      14,    15,    16,    17,   136,    31,    20,   139,    22,   141,
      26,    25,    26,     9,    31,   147,    30,   149,    32,     3,
       4,     5,     3,     4,     5,    21,    22,    23,    24,    21,
      22,    23,    24,     3,     4,     5,    20,    29,    22,    20,
      27,    22,    26,   112,    39,    26,    30,    31,    32,    30,
      20,    32,    22,     3,     4,     5,    26,    15,    -1,   115,
      30,    -1,    32,    -1,     3,     4,     5,     3,     4,     5,
      20,    -1,    22,    -1,    -1,    -1,    26,    -1,    -1,    -1,
      30,    20,    32,    22,    20,    -1,    22,    26,    -1,    -1,
      26,    30,    -1,    32,    30,    -1,    32,     3,     4,     5,
      21,    22,    23,    24,    21,    22,    23,    24,    -1,    -1,
      31,    -1,    -1,    -1,    20,    -1,    22,    -1,    -1,    -1,
      26,    -1,    -1,    -1,    30,    -1,    32
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    38,    39,     0,     3,     4,     5,     6,     8,    13,
      14,    15,    16,    17,    20,    22,    25,    26,    30,    32,
      40,    41,    43,    46,    47,    48,    51,    52,    53,    56,
      57,    12,    18,    28,    30,    32,    34,    50,    30,    30,
      30,    49,     5,    44,    44,    44,     5,    26,    30,    57,
      27,    42,     5,    56,    33,    40,    25,     9,    21,    22,
      23,    24,    30,    46,    56,    46,     5,    31,    56,    58,
      46,     5,    18,    21,    22,    23,    28,    20,    46,    55,
      43,    55,     5,    28,    29,    45,    40,    31,    31,    25,
      56,    56,    56,    56,     5,    29,    31,    33,    50,    46,
      46,    46,    46,    46,    10,    11,    35,    36,    31,    25,
      31,    30,    46,    44,    27,    30,    50,    31,    58,    50,
      46,    46,    46,    46,    41,    55,    41,     3,     4,     5,
      59,    60,    45,    59,    61,    28,     7,    54,    25,    29,
      31,    29,    31,    46,    41,    46,    58,    26,    58,    31,
      40,    41,    27
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    37,    39,    38,    40,    40,    41,    41,    41,    41,
      41,    41,    41,    42,    41,    43,    43,    43,    43,    44,
      44,    45,    45,    46,    46,    46,    46,    46,    46,    46,
      46,    46,    46,    47,    49,    48,    48,    50,    50,    50,
      50,    51,    52,    53,    54,    54,    55,    55,    55,    55,
      55,    55,    56,    56,    56,    56,    56,    56,    57,    57,
      57,    57,    57,    57,    57,    57,    57,    57,    58,    58,
      59,    59,    59,    60,    60,    61,    61
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     2,     1,     1,     1,
       1,     3,     1,     0,     4,     2,     2,     2,     1,     4,
       2,     2,     0,     3,     4,     6,     4,     4,     4,     2,
       1,     3,     4,     2,     0,     9,     6,     4,     3,     3,
       2,     5,     9,     6,     2,     0,     3,     3,     3,     3,
       1,     1,     3,     3,     3,     3,     1,     1,     1,     1,
       1,     3,     4,     3,     2,     2,     2,     1,     3,     1,
       1,     1,     1,     3,     1,     3,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2:
#line 59 "mini_js.y"
        {abrir_escopo();}
#line 1496 "y.tab.c"
    break;

  case 3:
#line 59 "mini_js.y"
                              {fechar_escopo(); imprimir_codigo(yyvsp[0].v + "." + funcoes);}
#line 1502 "y.tab.c"
    break;

  case 4:
#line 61 "mini_js.y"
                 {yyval.v = yyvsp[-1].v + yyvsp[0].v;}
#line 1508 "y.tab.c"
    break;

  case 11:
#line 70 "mini_js.y"
                        { yyval.v = yyvsp[-2].v + yyvsp[-1].v + "^"; }
#line 1514 "y.tab.c"
    break;

  case 12:
#line 71 "mini_js.y"
                    { yyval.v.clear(); }
#line 1520 "y.tab.c"
    break;

  case 13:
#line 72 "mini_js.y"
           {abrir_escopo();}
#line 1526 "y.tab.c"
    break;

  case 14:
#line 72 "mini_js.y"
                                       {fechar_escopo(); yyval.v = yyvsp[-1].v;}
#line 1532 "y.tab.c"
    break;

  case 15:
#line 75 "mini_js.y"
                                 {var_type = "let"; yyval.v = yyvsp[0].v;}
#line 1538 "y.tab.c"
    break;

  case 16:
#line 76 "mini_js.y"
                                 {var_type = "var"; yyval.v = yyvsp[0].v;}
#line 1544 "y.tab.c"
    break;

  case 17:
#line 77 "mini_js.y"
                                 {var_type = "const"; yyval.v = yyvsp[0].v;}
#line 1550 "y.tab.c"
    break;

  case 18:
#line 78 "mini_js.y"
                      {yyval.v = yyvsp[0].v + "^";}
#line 1556 "y.tab.c"
    break;

  case 19:
#line 81 "mini_js.y"
                              {declarar_var(yyvsp[-3].v[0]); yyval.v = yyvsp[-3].v + "&" + yyvsp[-3].v + yyvsp[-1].v + "="+"^" + yyvsp[0].v ; }
#line 1562 "y.tab.c"
    break;

  case 20:
#line 82 "mini_js.y"
                              {declarar_var(yyvsp[-1].v[0]); yyval.v = yyvsp[-1].v + "&" + yyvsp[0].v ;}
#line 1568 "y.tab.c"
    break;

  case 21:
#line 85 "mini_js.y"
                        { yyval.v = yyvsp[0].v; }
#line 1574 "y.tab.c"
    break;

  case 22:
#line 86 "mini_js.y"
                        { yyval.v.clear(); }
#line 1580 "y.tab.c"
    break;

  case 23:
#line 89 "mini_js.y"
                                  {verificar_var(yyvsp[-2].v[0]); yyval.v = yyvsp[-2].v + yyvsp[0].v + "=";}
#line 1586 "y.tab.c"
    break;

  case 24:
#line 90 "mini_js.y"
                                  {yyval.v = yyvsp[-3].v+ "@" + yyvsp[-2].v + yyvsp[0].v + "[=]"; }
#line 1592 "y.tab.c"
    break;

  case 25:
#line 91 "mini_js.y"
                                          {yyval.v = yyvsp[-4].v+ "@" + yyvsp[-2].v + yyvsp[0].v + "[=]"; }
#line 1598 "y.tab.c"
    break;

  case 26:
#line 92 "mini_js.y"
                                  {yyval.v = yyvsp[-3].v+ "@" + yyvsp[-2].v + "[@]"+ yyvsp[0].v + "+"; }
#line 1604 "y.tab.c"
    break;

  case 27:
#line 93 "mini_js.y"
                                  {yyval.v = yyvsp[-3].v+ "@" + yyvsp[-2].v + "[@]"+ yyvsp[0].v + "-"; }
#line 1610 "y.tab.c"
    break;

  case 28:
#line 94 "mini_js.y"
                                  {yyval.v = yyvsp[-3].v+ "@" + yyvsp[-2].v + "[@]"+ yyvsp[0].v + "*"; }
#line 1616 "y.tab.c"
    break;

  case 29:
#line 95 "mini_js.y"
                                  {yyval.v = yyvsp[-1].v+ "@" + yyvsp[0].v + "[@]";}
#line 1622 "y.tab.c"
    break;

  case 31:
#line 97 "mini_js.y"
                                  {yyval.v = yyvsp[-2].v + yyvsp[-2].v + "@" + yyvsp[0].v + "+"+ "="; }
#line 1628 "y.tab.c"
    break;

  case 32:
#line 98 "mini_js.y"
                                  { yyval.v = yyvsp[-3].v+ "@" + yyvsp[-2].v + yyvsp[-3].v+ "@" + yyvsp[-2].v + "[@]" + yyvsp[0].v + "+" + "[=]"; }
#line 1634 "y.tab.c"
    break;

  case 33:
#line 100 "mini_js.y"
                        {verificar_var(yyvsp[-1].v[0]); yyval.v = yyvsp[-1].v + "@" + yyvsp[-1].v + yyvsp[-1].v + "@" + "1" + "+" + "=" + "^"; }
#line 1640 "y.tab.c"
    break;

  case 34:
#line 103 "mini_js.y"
                       {abrir_escopo(); num_params = 0;}
#line 1646 "y.tab.c"
    break;

  case 35:
#line 103 "mini_js.y"
                                                                                           {
              fechar_escopo();
              // declarar_var($3.v[0]);
              string labelfunc = gera_label("LABELFUNCAO");
              yyval.v = yyvsp[-6].v + "&" + yyvsp[-6].v + "{}" + "=" + "'&funcao'" + labelfunc + "[=]" ;
              funcoes = funcoes + (":"+labelfunc) + yyvsp[-4].v +  "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" +yyvsp[-1].v +  "^"+ "undefined" + "@" + "'&retorno'" + "@" + "~";}
#line 1657 "y.tab.c"
    break;

  case 36:
#line 109 "mini_js.y"
                                          {num_params++; yyval.v = yyvsp[-1].v + to_string(num_params) +yyvsp[-4].v+ "@" +"$";}
#line 1663 "y.tab.c"
    break;

  case 37:
#line 113 "mini_js.y"
                                        { yyval.v = yyvsp[-2].v + "[@]" + yyvsp[0].v ; }
#line 1669 "y.tab.c"
    break;

  case 38:
#line 114 "mini_js.y"
                                        { yyval.v = yyvsp[-1].v + "[@]" + yyvsp[0].v; }
#line 1675 "y.tab.c"
    break;

  case 39:
#line 115 "mini_js.y"
                                        { yyval.v =  yyvsp[-1].v; }
#line 1681 "y.tab.c"
    break;

  case 40:
#line 116 "mini_js.y"
                                        { yyval.v = yyvsp[0].v; }
#line 1687 "y.tab.c"
    break;

  case 41:
#line 118 "mini_js.y"
                                    {
                              yyval.bloco = gera_label("LBL_LOOP");
                              yyval.end_bloco = gera_label("LBL_ENDWHILE");
                              yyval.v.clear(); yyval.v =  yyval.v + (":" +yyval.bloco) + yyvsp[-2].v + "!" +  yyval.end_bloco + "?" + yyvsp[0].v + yyval.bloco + "#" + (":" +  yyval.end_bloco);
                              }
#line 1697 "y.tab.c"
    break;

  case 42:
#line 125 "mini_js.y"
                                                   { 
                              yyval.bloco = gera_label("LBL_LOOP");
                              yyval.end_bloco = gera_label("LBL_ENDFOR");
                              yyval.v = yyvsp[-6].v + (":" +yyval.bloco) + yyvsp[-4].v + "!" + yyval.end_bloco + "?" + yyvsp[0].v + yyvsp[-2].v + "^" + yyval.bloco + "#" +(":" + yyval.end_bloco);}
#line 1706 "y.tab.c"
    break;

  case 43:
#line 130 "mini_js.y"
                                        {
                              yyval.bloco = gera_label("LBL_THEN");
                              yyval.end_bloco = gera_label("LBL_ENDIF");
                              yyval.v = yyvsp[-3].v + "!" + yyval.bloco+ "?" + yyvsp[-1].v + yyval.end_bloco + "#" + (":" + yyval.bloco) + yyvsp[0].v +(":" + yyval.end_bloco);
                              }
#line 1716 "y.tab.c"
    break;

  case 44:
#line 136 "mini_js.y"
                        {yyval.v = yyvsp[0].v; }
#line 1722 "y.tab.c"
    break;

  case 45:
#line 137 "mini_js.y"
                        { yyval.v.clear(); }
#line 1728 "y.tab.c"
    break;

  case 46:
#line 140 "mini_js.y"
                       { yyval.v = yyvsp[-2].v  + yyvsp[0].v + "<";}
#line 1734 "y.tab.c"
    break;

  case 47:
#line 141 "mini_js.y"
                      { yyval.v = yyvsp[-2].v  + yyvsp[0].v + ">";}
#line 1740 "y.tab.c"
    break;

  case 48:
#line 142 "mini_js.y"
                      { yyval.v = yyvsp[-2].v  + yyvsp[0].v + "==";}
#line 1746 "y.tab.c"
    break;

  case 49:
#line 143 "mini_js.y"
                      { yyval.v = yyvsp[-2].v  + yyvsp[0].v + "!=";}
#line 1752 "y.tab.c"
    break;

  case 52:
#line 148 "mini_js.y"
                {yyval.v = yyvsp[-2].v + yyvsp[0].v + "+";}
#line 1758 "y.tab.c"
    break;

  case 53:
#line 149 "mini_js.y"
                {yyval.v = yyvsp[-2].v + yyvsp[0].v + "-";}
#line 1764 "y.tab.c"
    break;

  case 54:
#line 150 "mini_js.y"
                {yyval.v = yyvsp[-2].v + yyvsp[0].v + "*";}
#line 1770 "y.tab.c"
    break;

  case 55:
#line 151 "mini_js.y"
                {yyval.v = yyvsp[-2].v + yyvsp[0].v + "/";}
#line 1776 "y.tab.c"
    break;

  case 58:
#line 157 "mini_js.y"
               {yyval.v = yyvsp[0].v + "@";}
#line 1782 "y.tab.c"
    break;

  case 61:
#line 160 "mini_js.y"
                    {yyval.v = yyvsp[-2].v + "#";}
#line 1788 "y.tab.c"
    break;

  case 62:
#line 161 "mini_js.y"
                            {num_params++; yyval.v = yyvsp[-1].v + to_string(num_params) +yyvsp[-3].v+ "@" +"$";}
#line 1794 "y.tab.c"
    break;

  case 63:
#line 162 "mini_js.y"
                    {yyval = yyvsp[-1];}
#line 1800 "y.tab.c"
    break;

  case 64:
#line 163 "mini_js.y"
                {yyval.v.clear(); yyval.v = yyval.v + "0" + yyvsp[0].v + yyvsp[-1].v;}
#line 1806 "y.tab.c"
    break;

  case 65:
#line 164 "mini_js.y"
                {yyval.v.clear(); yyval.v = yyval.v + "{}";}
#line 1812 "y.tab.c"
    break;

  case 66:
#line 165 "mini_js.y"
                {yyval.v.clear(); yyval.v = yyval.v + "[]";}
#line 1818 "y.tab.c"
    break;

  case 68:
#line 169 "mini_js.y"
                       {yyval.v = yyvsp[-2].v + yyvsp[0].v;}
#line 1824 "y.tab.c"
    break;

  case 73:
#line 178 "mini_js.y"
                                 {num_params++; yyval.v = yyvsp[-2].v + "&"+ yyvsp[-2].v + "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" + yyvsp[0].v;}
#line 1830 "y.tab.c"
    break;

  case 74:
#line 179 "mini_js.y"
                        {yyval.v = yyvsp[0].v + "&"+ yyvsp[0].v; }
#line 1836 "y.tab.c"
    break;

  case 75:
#line 182 "mini_js.y"
                                  {num_params++; yyval.v = yyvsp[-2].v + "@" + "arguments" + "@" + to_string(num_params) + "[@]" + "=" + "^" + yyvsp[0].v;}
#line 1842 "y.tab.c"
    break;

  case 76:
#line 183 "mini_js.y"
                        {yyval.v = yyvsp[0].v + "@"; }
#line 1848 "y.tab.c"
    break;


#line 1852 "y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 186 "mini_js.y"


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