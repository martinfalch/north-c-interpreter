  //| //|
 //||//|| Martin Falch, 2018.
// ||/ || GNU GPLv3

#include "tokenizer.h"

#define E  "([eE][+-]?[0-9]+)"
#define FS "[fFlL]"
#define IS "([uU][lL]|[lL][uU]|[uU]|[lL])"

Tokenizer::TokenMapping Tokenizer::tokenmap[] =
{
  {std::regex("^$"),      Token::Type::END_OF_FILE},
  {std::regex("^(\\[\\n]|[ \\s\\t\\r\\n])+"),
                Token::Type::IGNORE},
  {std::regex("^/\\*(.|[\r\n])*?\\*/"),
                Token::Type::COMMENT},
  {std::regex("^//[^\\n]*"),  Token::Type::COMMENT},

  {std::regex("^auto\\b"),  Token::Type::KEYWD_AUTO},
  {std::regex("^break\\b"), Token::Type::KEYWD_BREAK},
  {std::regex("^case\\b"),  Token::Type::KEYWD_CASE},
  {std::regex("^char\\b"),  Token::Type::KEYWD_CHAR},
  {std::regex("^const\\b"), Token::Type::KEYWD_CONST},
  {std::regex("^continue\\b"),Token::Type::KEYWD_CONTINUE},
  {std::regex("^default\\b"), Token::Type::KEYWD_DEFAULT},
  {std::regex("^do\\b"),    Token::Type::KEYWD_DO},
  {std::regex("^double\\b"),  Token::Type::KEYWD_DOUBLE},
  {std::regex("^else\\b"),  Token::Type::KEYWD_ELSE},
  {std::regex("^enum\\b"),  Token::Type::KEYWD_ENUM},
  {std::regex("^extern\\b"),  Token::Type::KEYWD_EXTERN},
  {std::regex("^float\\b"), Token::Type::KEYWD_FLOAT},
  {std::regex("^for\\b"),   Token::Type::KEYWD_FOR},
  {std::regex("^goto\\b"),  Token::Type::KEYWD_GOTO},
  {std::regex("^if\\b"),    Token::Type::KEYWD_IF},
  {std::regex("^int\\b"),   Token::Type::KEYWD_INT},
  {std::regex("^long\\b"),  Token::Type::KEYWD_LONG},
  {std::regex("^register\\b"),Token::Type::KEYWD_REGISTER},
  {std::regex("^return\\b"),  Token::Type::KEYWD_RETURN},
  {std::regex("^short\\b"), Token::Type::KEYWD_SHORT},
  {std::regex("^signed\\b"),  Token::Type::KEYWD_SIGNED},
  {std::regex("^sizeof\\b"),  Token::Type::KEYWD_SIZEOF},
  {std::regex("^static\\b"),  Token::Type::KEYWD_STATIC},
  {std::regex("^struct\\b"),  Token::Type::KEYWD_STRUCT},
  {std::regex("^switch\\b"),  Token::Type::KEYWD_SWITCH},
  {std::regex("^typedef\\b"), Token::Type::KEYWD_TYPEDEF},
  {std::regex("^union\\b"), Token::Type::KEYWD_UNION},
  {std::regex("^unsigned\\b"),Token::Type::KEYWD_UNSIGNED},
  {std::regex("^void\\b"),  Token::Type::KEYWD_VOID},
  {std::regex("^volatile\\b"),Token::Type::KEYWD_VOLATILE},
  {std::regex("^while\\b"), Token::Type::KEYWD_WHILE},

  {std::regex("^[_a-zA-Z][_a-zA-Z0-9]*"),  Token::Type::IDENTIFIER},

  {std::regex("^[0-9]+" E FS "?"),
                Token::Type::CONST_FLOATING},
  {std::regex("^[0-9]*[.][0-9]+(" E ")?" FS "?"),
                Token::Type::CONST_FLOATING},
  {std::regex("^[0-9]+[.][0-9]*(" E ")?" FS "?"),
                Token::Type::CONST_FLOATING},

  {std::regex("^0[xX][a-fA-F0-9]+" IS "?"),
                Token::Type::CONST_INTEGER_HEXADECIMAL},
  {std::regex("^0[0-7]*" IS "?"),
                Token::Type::CONST_INTEGER_OCTAL},
  {std::regex("^[1-9][0-9]*" IS "?"),
                Token::Type::CONST_INTEGER_DECIMAL},
  {std::regex("^'("
          "[^\\']|"
          "\\\\[abefnrtv\\'\"?]|"
          "\\\\0[0-7]{0,2}|"
          "\\\\x[a-fA-F0-9]{1,2}"
          ")'"),
                Token::Type::CONST_CHARACTER},

  {std::regex("^\"("
          "[^\\\\\"]|"
          "\\\\[abefnrtv\\'\"?]|"
          "\\\\0[0-7]{0,2}|"
          "\\\\x[a-fA-F0-9]{1,2}"
          ")*\""),
                Token::Type::STRING_LITERAL},

  {std::regex("^[\\.]{3}"), Token::Type::PUNCT_ELLIPSIS},
  {std::regex("^>>="),    Token::Type::PUNCT_RIGHT_SHIFT_ASSIGN},
  {std::regex("^<<="),    Token::Type::PUNCT_LEFT_SHIFT_ASSIGN},
  {std::regex("^\\+="),   Token::Type::PUNCT_ADD_ASSIGN},
  {std::regex("^-="),     Token::Type::PUNCT_SUBTRACT_ASSIGN},
  {std::regex("^\\*="),   Token::Type::PUNCT_MULTIPLY_ASSIGN},
  {std::regex("^/="),     Token::Type::PUNCT_DIVIDE_ASSIGN},
  {std::regex("^%="),     Token::Type::PUNCT_MODULO_ASSIGN},
  {std::regex("^&="),     Token::Type::PUNCT_AND_ASSIGN},
  {std::regex("^\\^="),   Token::Type::PUNCT_XOR_ASSIGN},
  {std::regex("^\\|="),   Token::Type::PUNCT_OR_ASSIGN},
  {std::regex("^>>"),     Token::Type::PUNCT_RIGHT_SHIFT},
  {std::regex("^<<"),     Token::Type::PUNCT_LEFT_SHIFT},
  {std::regex("^\\+\\+"),   Token::Type::PUNCT_INCREMENT},
  {std::regex("^--"),     Token::Type::PUNCT_DECREMENT},
  {std::regex("^->"),     Token::Type::PUNCT_POINTER_MEMBER},
  {std::regex("^&&"),     Token::Type::PUNCT_LOGIC_AND},
  {std::regex("^\\|\\|"),   Token::Type::PUNCT_LOGIC_OR},
  {std::regex("^<="),     Token::Type::PUNCT_LESS_OR_EQUAL},
  {std::regex("^>="),     Token::Type::PUNCT_GREATER_OR_EQUAL},
  {std::regex("^=="),     Token::Type::PUNCT_EQUAL},
  {std::regex("^!="),     Token::Type::PUNCT_NOT_EQUAL},
  {std::regex("^;"),      Token::Type::PUNCT_SEMICOLON},
  {std::regex("^\\{"),    Token::Type::PUNCT_START_BLOCK},
  {std::regex("^\\}"),    Token::Type::PUNCT_END_BLOCK},
  {std::regex("^,"),      Token::Type::PUNCT_COMMA},
  {std::regex("^:"),      Token::Type::PUNCT_COLON},
  {std::regex("^="),      Token::Type::PUNCT_ASSIGN},
  {std::regex("^\\("),    Token::Type::PUNCT_LEFT_PARENS},
  {std::regex("^\\)"),    Token::Type::PUNCT_RIGHT_PARENS},
  {std::regex("^\\["),    Token::Type::PUNCT_LEFT_BRACKETS},
  {std::regex("^\\]"),    Token::Type::PUNCT_RIGHT_BRACKETS},
  {std::regex("^\\."),    Token::Type::PUNCT_MEMBER},
  {std::regex("^&"),      Token::Type::PUNCT_AMPERSAND},
  {std::regex("^!"),      Token::Type::PUNCT_LOGIC_NOT},
  {std::regex("^~"),      Token::Type::PUNCT_BITWISE_NOT},
  {std::regex("^-"),      Token::Type::PUNCT_MINUS},
  {std::regex("^\\+"),    Token::Type::PUNCT_PLUS},
  {std::regex("^\\*"),    Token::Type::PUNCT_STAR},
  {std::regex("^/"),      Token::Type::PUNCT_DIVIDE},
  {std::regex("^%"),      Token::Type::PUNCT_MODULO},
  {std::regex("^<"),      Token::Type::PUNCT_LESS},
  {std::regex("^>"),      Token::Type::PUNCT_GREATER},
  {std::regex("^\\^"),    Token::Type::PUNCT_BITWISE_XOR},
  {std::regex("^\\|"),    Token::Type::PUNCT_BITWISE_OR},
  {std::regex("^\\?"),    Token::Type::PUNCT_QUESTION_MARK},

  {std::regex("^."),      Token::Type::UNKNOWN},
};

std::ostream& operator<<(std::ostream& o, const Token& t)
{
  for (const char* c = t.begin; c != t.end; c++)
  {
    o << *c;
  }
  return o;
}

std::ostream& operator<<(std::ostream& o, const Tokenizer::Location& l)
{
  o << l.line << ',' << l.column;
  return o;
}

