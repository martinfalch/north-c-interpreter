  //| //|
 //||//|| Martin Falch, 2018.
// ||/ || GNU GPLv3

#pragma once

#include <cstring>
#include <string>
#include <regex>
#include <stdexcept>
#include <ostream>

#include "circularbuffer.h"

class Token
{
  public:
  enum class Type
  {
    UNKNOWN = 0,
    IGNORE,
    COMMENT,

    KEYWD_AUTO = 100,
    KEYWD_BREAK,
    KEYWD_CASE,
    KEYWD_CHAR,
    KEYWD_CONST,
    KEYWD_CONTINUE,
    KEYWD_DEFAULT,
    KEYWD_DO,
    KEYWD_DOUBLE,
    KEYWD_ELSE,
    KEYWD_ENUM,
    KEYWD_EXTERN,
    KEYWD_FLOAT,
    KEYWD_FOR,
    KEYWD_GOTO,
    KEYWD_IF,
    KEYWD_INLINE,
    KEYWD_INT,
    KEYWD_LONG,
    KEYWD_REGISTER,
    KEYWD_RESTRICT,
    KEYWD_RETURN,
    KEYWD_SHORT,
    KEYWD_SIGNED,
    KEYWD_SIZEOF,
    KEYWD_STATIC,
    KEYWD_STRUCT,
    KEYWD_SWITCH,
    KEYWD_TYPEDEF,
    KEYWD_UNION,
    KEYWD_UNSIGNED,
    KEYWD_VOID,
    KEYWD_VOLATILE,
    KEYWD_WHILE,
    KEYWD_BOOL,
    KEYWD_COMPLEX,
    KEYWD_IMAGINARY,

    IDENTIFIER = 200,

    CONST_INTEGER_HEXADECIMAL = 300,
    CONST_INTEGER_OCTAL,
    CONST_INTEGER_DECIMAL,
    CONST_CHARACTER,
    CONST_FLOATING,
    STRING_LITERAL,

    PUNCT_ELLIPSIS = 400,
    PUNCT_RIGHT_SHIFT_ASSIGN,
    PUNCT_LEFT_SHIFT_ASSIGN,
    PUNCT_ADD_ASSIGN,
    PUNCT_SUBTRACT_ASSIGN,
    PUNCT_MULTIPLY_ASSIGN,
    PUNCT_DIVIDE_ASSIGN,
    PUNCT_MODULO_ASSIGN,
    PUNCT_AND_ASSIGN,
    PUNCT_XOR_ASSIGN,
    PUNCT_OR_ASSIGN,
    PUNCT_RIGHT_SHIFT,
    PUNCT_LEFT_SHIFT,
    PUNCT_INCREMENT,
    PUNCT_DECREMENT,
    PUNCT_POINTER_MEMBER,
    PUNCT_LOGIC_AND,
    PUNCT_LOGIC_OR,
    PUNCT_LESS_OR_EQUAL,
    PUNCT_GREATER_OR_EQUAL,
    PUNCT_EQUAL,
    PUNCT_NOT_EQUAL,
    PUNCT_SEMICOLON,
    PUNCT_START_BLOCK,
    PUNCT_END_BLOCK,
    PUNCT_COMMA,
    PUNCT_COLON,
    PUNCT_ASSIGN,
    PUNCT_LEFT_PARENS,
    PUNCT_RIGHT_PARENS,
    PUNCT_LEFT_BRACKETS,
    PUNCT_RIGHT_BRACKETS,
    PUNCT_MEMBER,
    PUNCT_AMPERSAND,
    PUNCT_LOGIC_NOT,
    PUNCT_BITWISE_NOT,
    PUNCT_MINUS,
    PUNCT_PLUS,
    PUNCT_STAR,
    PUNCT_DIVIDE,
    PUNCT_MODULO,
    PUNCT_LESS,
    PUNCT_GREATER,
    PUNCT_BITWISE_XOR,
    PUNCT_BITWISE_OR,
    PUNCT_QUESTION_MARK,
    PUNCT_HASH,

    BACKTRACK = 500,
    END_OF_FILE = 501
  };

  constexpr Token()
    :
    type(Type::UNKNOWN),
    begin(nullptr),
    end(nullptr)
  { }

  constexpr Token(Type t, const char* const& begin, std::size_t size)
    :
    type(t),
    begin(begin),
    end(begin+size)
  { }

  constexpr operator bool() const
  {
    return (type != Type::END_OF_FILE) && (type != Type::UNKNOWN);
  }

  bool operator==(const Token& t) const
  {
    return (type == t.type)
      && ((end-begin) == (t.end-t.begin))
      && (std::strncmp(begin, t.begin, end-begin) == 0);
  }

  Type type;
  const char* begin;
  const char* end;
};

std::ostream& operator<<(std::ostream& o, const Token& t);

class Tokenizer
{
  public:
  struct TokenMapping
  {
    std::regex regex;
    Token::Type type;
  };

  struct Location
  {
    int line;
    int column;
  };

  struct Error : public std::runtime_error
  {
    Error(const char* what_arg, Location&& location)
      :
      std::runtime_error(what_arg),
      location(location)
    { }

    Location location;
  };

  Tokenizer(const char* buffer)
    :
    size(std::strlen(buffer)),
    buffer(buffer),
    position(buffer)
  { }

  Location location(const Token& t) const
  {
    return location(t.begin);
  }

  Location location(const char* pos) const
  {
    Location result = {1, 1}; // {line, column}
    const char* curr = buffer;

    while (curr < pos)
    {
      ++result.column;
      if (*curr == '\n')
      {
        ++result.line;
        result.column = 1;
      }
      ++curr;
    }
    return result;
  }

  void backtrack(const Token& t)
  {
    position = t.begin;
    if ((position < buffer) || (position > &buffer[size]))
      throw Error("Corrupted token in backtrack.", location(t));
  }

  Token backtrack_token() const
  {
    return Token(Token::Type::BACKTRACK, position, 0);
  }

  Token next()
  {
    // end of file:
    if (position >= &buffer[size])
    {
      return Token(Token::Type::END_OF_FILE, position, 0);
    }

    // token map:
    for (;;)
    {
      for (auto& map : Tokenizer::tokenmap)
      {
        std::cmatch match;
        if (std::regex_search(position, match, map.regex))
        {
          if (map.type == Token::Type::IGNORE)
          {
            position += match.length();
            break;
          }
          else
          { Token t = Token(map.type, position, match.length());
            position += match.length();
            return t;
          }
        }
      }
    }

  }

  std::size_t size;
  const char* buffer;
  const char* position;

  static TokenMapping tokenmap[92];
};

template <unsigned int DEPTH>
class LookAheadTokenizer : public Tokenizer
{
  public:
  using Tokenizer::Tokenizer;

  void backtrack(const Token& token)
  {
    Tokenizer::backtrack(token);
    queue.clear();
  }

  Token backtrack_token() const
  {
    if (queue.size() > 0)
    {
      return Token(Token::Type::BACKTRACK, queue.peek(0).begin, 0);
    }
    else
    {
      return Tokenizer::backtrack_token();
    }
  }

  Token next()
  {
    if (queue.size() > 0)
    {
      return queue.pop();
    }
    else
    {
      return Tokenizer::next();
    }
  }

  template <int STEPS_AHEAD>
  Token peek()
  {
    static_assert(STEPS_AHEAD < DEPTH, "Too deep peek requested.");

    while (queue.size() <= STEPS_AHEAD)
    {
      queue.push(Tokenizer::next());
    }

    return queue.peek(STEPS_AHEAD);
  }

  protected:
  CircularBuffer<Token, DEPTH> queue;
};

inline bool operator==(const Tokenizer::Location& l1,
  const Tokenizer::Location& l2)
{
  return (l1.line == l2.line) && (l1.column == l2.column);
}

std::ostream& operator<<(std::ostream& o, const Tokenizer::Location& l);

