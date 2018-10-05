  //| //|
 //||//|| Martin Falch, 2018.
// ||/ || GNU GPLv3

#pragma once

#include "syntaxtree.h"

class Parser
{
  public:
  struct Error : public std::runtime_error
  {
    Error(const char* what_arg, const Tokenizer::Location& location)
      :
      std::runtime_error(what_arg),
      location(location)
    { }

    Tokenizer::Location location;
  };

  Parser(const char* source, SyntaxTree& tree)
    :
    tokenizer(source),
    tree(tree)
  {
    tree.translation_unit = translation_unit();
  }

  protected:
  using TokenizerType = LookAheadTokenizer<2>;
  template <typename T>
  using Handle = SyntaxTree::Handle<T>;


  template <typename T>
  struct Backtracker
  {
    Backtracker(const Backtracker<T>&) = delete;
    Backtracker(Backtracker<T>&&) = delete;

    Backtracker(Handle<T>&& handle,
      TokenizerType& tokenizer)
      :
      handle(std::move(handle)),
      tokenizer(tokenizer),
      token(tokenizer.backtrack_token())
    { }

    constexpr T* operator->()
    {
      if (!handle)
        throw Error("Uninitialized Parser::Backtracker.",
          tokenizer.location(token));
      return handle.operator->();
    }

    constexpr const T* operator->() const
    {
      if (!handle)
        throw Error("Uninitialized Parser::Backtracker.",
          tokenizer.location(token));
      return handle.operator->();
    }

    Handle<T> keep()
    {
      return std::move(handle);
    }

    ~Backtracker()
    {
      if (handle)
      {
        handle.tree->remove(std::move(handle));
        tokenizer.backtrack(token);
      }
    }

    Handle<T> handle;
    TokenizerType& tokenizer;
    Token token;
  };

  template <typename T>
  Backtracker<T> backtracker(Handle<T>&& handle)
  {
    return Backtracker(std::move(handle), tokenizer);
  }

  void throw_error(const char* message, const Token& token)
  {
    throw Error(message, tokenizer.location(token));
  }

  void set_once(Token& token, const Token& value, const char* message)
  {
    if (token)
    {
      throw_error(message, value);
    }
    token = value;
  }

  template <typename T>
  void set_once(Handle<T>& handle, Handle<T>&& value,
    const char* message)
  {
    if (handle)
    {
      throw_error(message, tokenizer.backtrack_token());
    }
    handle = std::move(value);
  }

  template <typename T, typename... Args>
  void set_once(std::variant<Args...>& variant, Handle<T>&& value,
    const char* message)
  {
    if (std::visit([](auto&& handle) -> bool { return handle; }, variant))
    {
      throw_error(message, tokenizer.backtrack_token());
    }
    variant = std::move(value);
  }

  // B.2.1 Expressions
  Handle<SyntaxTree::Expression> primary_expression();
  Handle<SyntaxTree::Expression> postfix_expression();
  Handle<SyntaxTree::Expression> argument_expression_list();
  Handle<SyntaxTree::Expression> unary_expression();
  void unary_operator();
  Handle<SyntaxTree::Expression> cast_expression();
  Handle<SyntaxTree::Expression> multiplicative_expression();
  Handle<SyntaxTree::Expression> additive_expression();
  Handle<SyntaxTree::Expression> shift_expression();
  Handle<SyntaxTree::Expression> relational_expression();
  Handle<SyntaxTree::Expression> equality_expression();
  Handle<SyntaxTree::Expression> and_expression();
  Handle<SyntaxTree::Expression> exclusive_or_expression();
  Handle<SyntaxTree::Expression> inclusive_or_expression();
  Handle<SyntaxTree::Expression> logical_and_expression();
  Handle<SyntaxTree::Expression>  logical_or_expression();
  Handle<SyntaxTree::Expression>
    conditional_expression();
  Handle<SyntaxTree::Expression> assignment_expression();
  void assignment_operator();
  Handle<SyntaxTree::Expression> expression();
  Handle<SyntaxTree::ConstantExpression> constant_expression();

  // B.2.2 Declarations
  Handle<SyntaxTree::Declaration> declaration();
  Handle<SyntaxTree::DeclarationSpecifiers>
    declaration_specifiers();
  Handle<SyntaxTree::InitDeclarator> init_declarator_list();
  Handle<SyntaxTree::InitDeclarator> init_declarator();
  void storage_class_specifier();
  void type_specifier();
  Handle<SyntaxTree::StructUnion> struct_or_union_specifier();
  void struct_or_union();
  Handle<SyntaxTree::StructDeclaration> struct_declaration_list();
  Handle<SyntaxTree::StructDeclaration> struct_declaration();
  Handle<SyntaxTree::DeclarationSpecifiers> specifier_qualifier_list();
  Handle<SyntaxTree::StructDeclarator> struct_declarator_list();
  Handle<SyntaxTree::StructDeclarator> struct_declarator();
  Handle<SyntaxTree::Enum> enum_specifier();
  Handle<SyntaxTree::Enumerator> enumerator_list();
  Handle<SyntaxTree::Enumerator> enumerator();
  void type_qualifier();
  Handle<SyntaxTree::Declarator> declarator();
  Handle<SyntaxTree::DirectDeclarator> direct_declarator();
  Handle<SyntaxTree::Pointer> pointer();
  SyntaxTree::Qualifiers type_qualifier_list();
  Handle<SyntaxTree::ParameterDeclaration> parameter_type_list();
  Handle<SyntaxTree::ParameterDeclaration> parameter_list();
  Handle<SyntaxTree::ParameterDeclaration> parameter_declaration();
  void identifier_list();
  Handle<SyntaxTree::TypeName> type_name();
  Handle<SyntaxTree::Declarator> abstract_declarator();
  Handle<SyntaxTree::DirectDeclarator> direct_abstract_declarator();
  void typedef_name();
  Handle<SyntaxTree::Initializer> initializer();
  Handle<SyntaxTree::InitializerList> initializer_list();

  // B.2.3 Statements
  Handle<SyntaxTree::Statement> statement();
  Handle<SyntaxTree::Statement> labeled_statement();
  Handle<SyntaxTree::CompoundStatement> compound_statement();
  Handle<SyntaxTree::Declaration> declaration_list();
  Handle<SyntaxTree::Statement> statement_list();
  Handle<SyntaxTree::ExpressionStatement> expression_statement();
  Handle<SyntaxTree::Statement> selection_statement();
  Handle<SyntaxTree::Statement> iteration_statement();
  Handle<SyntaxTree::Statement> jump_statement();

  // B.2.4 External definitions
  Handle<SyntaxTree::ExternalDeclaration> translation_unit();
  Handle<SyntaxTree::ExternalDeclaration> external_declaration();
  Handle<SyntaxTree::FunctionDefinition> function_definition();

  TokenizerType tokenizer;
  SyntaxTree& tree;
};

DynamicSyntaxTree parse(const char* source);
void parser(const char* source, SyntaxTree& tree);

