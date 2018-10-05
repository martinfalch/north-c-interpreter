  //| //|
 //||//|| Martin Falch, 2018.
// ||/ || GNU GPLv3

#include "parser.h"

template <typename T>
using Handle = Parser::Handle<T>;

template <typename T>
Handle<T>& end_of_list(Handle<T>& first)
{
  auto* current = &first;
  while ((*current)->next)
  {
    current = &(*current)->next;
  }

  return *current;
}

template <typename T>
Handle<T>& end_of_list(Parser::Backtracker<T>& first)
{
  auto* current = &first.handle;
  while ((*current)->next)
  {
    current = &(*current)->next;
  }

  return *current;
}

static bool is_postfix_operator(const Token& token)
{
  bool result;

  switch (token.type)
  {
    case Token::Type::PUNCT_LEFT_BRACKETS:
    case Token::Type::PUNCT_LEFT_PARENS:
    case Token::Type::PUNCT_MEMBER:
    case Token::Type::PUNCT_POINTER_MEMBER:
    case Token::Type::PUNCT_INCREMENT:
    case Token::Type::PUNCT_DECREMENT:
      result = true;
      break;

    default:
      result = false;
      break;
  }

  return result;
}

/*
static bool is_unary_operator(const Token& token)
{
  bool result;

  switch (token.type)
  {
    case Token::Type::PUNCT_AMPERSAND:
    case Token::Type::PUNCT_STAR:
    case Token::Type::PUNCT_PLUS:
    case Token::Type::PUNCT_MINUS:
    case Token::Type::PUNCT_BITWISE_NOT:
    case Token::Type::PUNCT_LOGIC_NOT:
      result = true;
      break;

    default:
      result = false;
      break;
  }

  return result;
}
*/

static bool is_assignment_operator(const Token& token)
{
  bool result;

  switch (token.type)
  {
    case Token::Type::PUNCT_ASSIGN:
    case Token::Type::PUNCT_MULTIPLY_ASSIGN:
    case Token::Type::PUNCT_DIVIDE_ASSIGN:
    case Token::Type::PUNCT_MODULO_ASSIGN:
    case Token::Type::PUNCT_ADD_ASSIGN:
    case Token::Type::PUNCT_SUBTRACT_ASSIGN:
    case Token::Type::PUNCT_LEFT_SHIFT_ASSIGN:
    case Token::Type::PUNCT_RIGHT_SHIFT_ASSIGN:
    case Token::Type::PUNCT_AND_ASSIGN:
    case Token::Type::PUNCT_XOR_ASSIGN:
    case Token::Type::PUNCT_OR_ASSIGN:
      result = true;
      break;

    default:
      result = false;
      break;
  }

  return result;
}

static bool is_declaration_specifier(const Token& token)
{
  bool result;

  switch (token.type)
  {
    case Token::Type::KEYWD_AUTO:
    case Token::Type::KEYWD_REGISTER:
    case Token::Type::KEYWD_STATIC:
    case Token::Type::KEYWD_EXTERN:

    case Token::Type::KEYWD_CONST:
    case Token::Type::KEYWD_VOLATILE:

    case Token::Type::KEYWD_UNSIGNED:
    case Token::Type::KEYWD_SIGNED:
    case Token::Type::KEYWD_VOID:
    case Token::Type::KEYWD_CHAR:
    case Token::Type::KEYWD_SHORT:
    case Token::Type::KEYWD_INT:
    case Token::Type::KEYWD_LONG:
    case Token::Type::KEYWD_FLOAT:
    case Token::Type::KEYWD_DOUBLE:

    case Token::Type::KEYWD_STRUCT:
    case Token::Type::KEYWD_UNION:
    case Token::Type::KEYWD_ENUM:
      result = true;
      break;

    default:
      result = false;
      break;
  }

  return result;
}

static bool is_type_qualifier(const Token& token)
{
  switch (token.type)
  {
    case Token::Type::KEYWD_CONST:
    case Token::Type::KEYWD_VOLATILE:
      return true;

    default:
      return false;
  }
}

Handle<SyntaxTree::Expression>
Parser::primary_expression()
{
  switch (tokenizer.peek<0>().type)
  {
    case Token::Type::IDENTIFIER:
    case Token::Type::CONST_INTEGER_HEXADECIMAL:
    case Token::Type::CONST_INTEGER_OCTAL:
    case Token::Type::CONST_INTEGER_DECIMAL:
    case Token::Type::CONST_CHARACTER:
    case Token::Type::CONST_FLOATING:
    case Token::Type::STRING_LITERAL:
      {
        auto handle = tree.add<SyntaxTree::Expression>();
        auto expr = tree.add<SyntaxTree::PrimaryExpression>(handle->variant);
        expr->token = tokenizer.next();
        return handle;
      }
      break;

    case Token::Type::PUNCT_LEFT_PARENS:
      {
        auto bt = tokenizer.backtrack_token();
        tokenizer.next();
        auto handle = expression();
        if (tokenizer.next().type == Token::Type::PUNCT_RIGHT_PARENS)
        {
          return handle;
        }
        tokenizer.backtrack(bt);
      }
      break;

    default:
      break;
  }
  return {};
}

Handle<SyntaxTree::Expression>
Parser::postfix_expression()
{
  auto current = primary_expression();

  if (current)
  {
    while (is_postfix_operator(tokenizer.peek<0>()))
    {
      switch (tokenizer.peek<0>().type)
      {
        case Token::Type::PUNCT_LEFT_BRACKETS:
          {
            auto bt = backtracker(tree.add<SyntaxTree::Expression>());
            auto handle = tree.add<SyntaxTree::ArraySubscriptExpression>(
              bt->variant);

            tokenizer.next();
            handle->expressions.at(1) = expression();
            if (handle->expressions.at(1) &&
              (tokenizer.next().type == Token::Type::PUNCT_RIGHT_BRACKETS))
            {
              handle->expressions.at(0) = std::move(current);
              current = bt.keep();
            }
          }
          break;

        case Token::Type::PUNCT_LEFT_PARENS:
          {
            auto bt = backtracker(tree.add<SyntaxTree::Expression>());
            auto handle = tree.add<SyntaxTree::FunctionCallExpression>(
              bt->variant);

            tokenizer.next();
            handle->expressions.at(1) = argument_expression_list();
            if (tokenizer.next().type == Token::Type::PUNCT_RIGHT_PARENS)
            {
              handle->expressions.at(0) = std::move(current);
              current = bt.keep();
            }
          }
          break;

        case Token::Type::PUNCT_MEMBER:
        case Token::Type::PUNCT_POINTER_MEMBER:
          {
            auto bt = backtracker(tree.add<SyntaxTree::Expression>());
            auto handle = tree.add<SyntaxTree::MemberAccessExpression>(
              bt->variant);

            handle->type = tokenizer.next();
            if (tokenizer.peek<0>().type == Token::Type::IDENTIFIER)
            {
              handle->expression = std::move(current);
              handle->identifier = SyntaxTree::Identifier{tokenizer.next()};
              current = bt.keep();
            }
          }
          break;

        case Token::Type::PUNCT_INCREMENT:
        case Token::Type::PUNCT_DECREMENT:
          {
            auto expr = tree.add<SyntaxTree::Expression>();
            auto handle = tree.add<SyntaxTree::PostfixExpression>(
              expr->variant);

            handle->type = tokenizer.next();
            handle->expression = std::move(current);
            current = std::move(expr);
          }

          break;

        default:
          break;
      }
    }
  }

  return current;
}

Handle<SyntaxTree::Expression>
Parser::argument_expression_list()
{
  auto first = assignment_expression();
  if (first)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_COMMA)
    {
      tokenizer.next();
      auto& eol = end_of_list(first);
      eol->next = assignment_expression();
      if (!eol->next)
      {
        throw_error("Expected assignment-expression.",
          tokenizer.next());
      }
    }
  }
  return first;
}

Handle<SyntaxTree::Expression>
Parser::unary_expression()
{
  if (auto handle = postfix_expression())
  {
    return handle;
  }
  switch (tokenizer.peek<0>().type)
  {
    case Token::Type::PUNCT_INCREMENT:
    case Token::Type::PUNCT_DECREMENT:
      {
        auto bt = backtracker(tree.add<SyntaxTree::Expression>());
        auto handle = tree.add<SyntaxTree::UnaryExpression>();

        handle->type = tokenizer.next();
        auto expr = unary_expression();
        if (expr)
        {
          handle->variant = std::move(expr);
          bt->variant = std::move(handle);
          return bt.keep();
        }
      }
      break;

    case Token::Type::PUNCT_AMPERSAND:
    case Token::Type::PUNCT_STAR:
    case Token::Type::PUNCT_PLUS:
    case Token::Type::PUNCT_MINUS:
    case Token::Type::PUNCT_BITWISE_NOT:
    case Token::Type::PUNCT_LOGIC_NOT:
      {
        auto bt = backtracker(tree.add<SyntaxTree::Expression>());
        auto handle = tree.add<SyntaxTree::UnaryExpression>();

        handle->type = tokenizer.next();
        auto expr = cast_expression();
        if (expr)
        {
          handle->variant = std::move(expr);
          bt->variant = std::move(handle);
          return bt.keep();
        }
      }
      break;

    case Token::Type::KEYWD_SIZEOF:
      {
        auto bt = backtracker(tree.add<SyntaxTree::Expression>());
        auto handle = tree.add<SyntaxTree::UnaryExpression>();

        handle->type = tokenizer.next();
        auto expr = unary_expression();
        if (expr)
        {
          handle->variant = std::move(expr);
          bt->variant = std::move(handle);
          return bt.keep();
        }
        if (tokenizer.next().type == Token::Type::PUNCT_LEFT_PARENS)
        {
          auto tname = type_name();
          if (tname &&
            (tokenizer.next().type == Token::Type::PUNCT_RIGHT_PARENS))
          {
            handle->variant = std::move(tname);
            bt->variant = std::move(handle);
            return bt.keep();
          }
        }
      }
      break;

    default:
      break;
  }

  return {};
}

Handle<SyntaxTree::Expression>
Parser::cast_expression()
{
  if (auto handle = unary_expression())
  {
    return handle;
  }
  if (tokenizer.peek<0>().type == Token::Type::PUNCT_LEFT_PARENS)
  {
    auto bt = backtracker(tree.add<SyntaxTree::Expression>());
    auto handle = tree.add<SyntaxTree::CastExpression>();

    tokenizer.next();
    handle->type_name = type_name();
    if (handle->type_name &&
      (tokenizer.next().type == Token::Type::PUNCT_RIGHT_PARENS))
    {
      handle->expression = cast_expression();
      if (handle->expression)
      {
        bt->variant = std::move(handle);
        return bt.keep();
      }
    }
  }
  return {};
}

Handle<SyntaxTree::Expression>
Parser::multiplicative_expression()
{
  auto current = cast_expression();

  if (current)
  {
    while ((tokenizer.peek<0>().type == Token::Type::PUNCT_STAR) ||
      (tokenizer.peek<0>().type == Token::Type::PUNCT_DIVIDE) ||
      (tokenizer.peek<0>().type == Token::Type::PUNCT_MODULO))
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto bexpr = tree.add<SyntaxTree::BinaryExpression>();
      bexpr->type = tokenizer.next();
      bexpr->expressions.at(1) = cast_expression();
      if (bexpr->expressions.at(1))
      {
        bexpr->expressions.at(0) = std::move(current);
        bt->variant = std::move(bexpr);
        current = bt.keep();
      }
    }
  }
  return current;
}

Handle<SyntaxTree::Expression>
Parser::additive_expression()
{
  auto current = multiplicative_expression();

  if (current)
  {
    while ((tokenizer.peek<0>().type == Token::Type::PUNCT_PLUS) ||
      (tokenizer.peek<0>().type == Token::Type::PUNCT_MINUS))
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto bexpr = tree.add<SyntaxTree::BinaryExpression>();
      bexpr->type = tokenizer.next();
      bexpr->expressions.at(1) = multiplicative_expression();
      if (bexpr->expressions.at(1))
      {
        bexpr->expressions.at(0) = std::move(current);
        bt->variant = std::move(bexpr);
        current = bt.keep();
      }
    }
  }
  return current;
}

Handle<SyntaxTree::Expression>
Parser::shift_expression()
{
  auto current = additive_expression();

  if (current)
  {
    while ((tokenizer.peek<0>().type == Token::Type::PUNCT_LEFT_SHIFT) ||
      (tokenizer.peek<0>().type == Token::Type::PUNCT_RIGHT_SHIFT))
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto bexpr = tree.add<SyntaxTree::BinaryExpression>();
      bexpr->type = tokenizer.next();
      bexpr->expressions.at(1) = additive_expression();
      if (bexpr->expressions.at(1))
      {
        bexpr->expressions.at(0) = std::move(current);
        bt->variant = std::move(bexpr);
        current = bt.keep();
      }
    }
  }
  return current;
}

Handle<SyntaxTree::Expression>
Parser::relational_expression()
{
  auto current = shift_expression();

  if (current)
  {
    while ((tokenizer.peek<0>().type == Token::Type::PUNCT_LESS) ||
      (tokenizer.peek<0>().type == Token::Type::PUNCT_GREATER) ||
      (tokenizer.peek<0>().type == Token::Type::PUNCT_LESS_OR_EQUAL) ||
      (tokenizer.peek<0>().type == Token::Type::PUNCT_GREATER_OR_EQUAL))
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto bexpr = tree.add<SyntaxTree::BinaryExpression>();
      bexpr->type = tokenizer.next();
      bexpr->expressions.at(1) = shift_expression();
      if (bexpr->expressions.at(1))
      {
        bexpr->expressions.at(0) = std::move(current);
        bt->variant = std::move(bexpr);
        current = bt.keep();
      }
    }
  }
  return current;
}

Handle<SyntaxTree::Expression>
Parser::equality_expression()
{
  auto current = relational_expression();

  if (current)
  {
    while ((tokenizer.peek<0>().type == Token::Type::PUNCT_EQUAL) ||
      (tokenizer.peek<0>().type == Token::Type::PUNCT_NOT_EQUAL))
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto bexpr = tree.add<SyntaxTree::BinaryExpression>();
      bexpr->type = tokenizer.next();
      bexpr->expressions.at(1) = relational_expression();
      if (bexpr->expressions.at(1))
      {
        bexpr->expressions.at(0) = std::move(current);
        bt->variant = std::move(bexpr);
        current = bt.keep();
      }
    }
  }
  return current;
}

Handle<SyntaxTree::Expression>
Parser::and_expression()
{
  auto current = equality_expression();

  if (current)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_AMPERSAND)
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto bexpr = tree.add<SyntaxTree::BinaryExpression>();
      bexpr->type = tokenizer.next();
      bexpr->expressions.at(1) = equality_expression();
      if (bexpr->expressions.at(1))
      {
        bexpr->expressions.at(0) = std::move(current);
        bt->variant = std::move(bexpr);
        current = bt.keep();
      }
    }
  }
  return current;
}

Handle<SyntaxTree::Expression>
Parser::exclusive_or_expression()
{
  auto current = and_expression();

  if (current)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_BITWISE_XOR)
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto bexpr = tree.add<SyntaxTree::BinaryExpression>();
      bexpr->type = tokenizer.next();
      bexpr->expressions.at(1) = and_expression();
      if (bexpr->expressions.at(1))
      {
        bexpr->expressions.at(0) = std::move(current);
        bt->variant = std::move(bexpr);
        current = bt.keep();
      }
    }
  }
  return current;
}

Handle<SyntaxTree::Expression>
Parser::inclusive_or_expression()
{
  auto current = exclusive_or_expression();

  if (current)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_BITWISE_OR)
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto bexpr = tree.add<SyntaxTree::BinaryExpression>();
      bexpr->type = tokenizer.next();
      bexpr->expressions.at(1) = exclusive_or_expression();
      if (bexpr->expressions.at(1))
      {
        bexpr->expressions.at(0) = std::move(current);
        bt->variant = std::move(bexpr);
        current = bt.keep();
      }
    }
  }
  return current;
}

Handle<SyntaxTree::Expression>
Parser::logical_and_expression()
{
  auto current = inclusive_or_expression();

  if (current)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_LOGIC_AND)
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto bexpr = tree.add<SyntaxTree::BinaryExpression>();
      bexpr->type = tokenizer.next();
      bexpr->expressions.at(1) = inclusive_or_expression();
      if (bexpr->expressions.at(1))
      {
        bexpr->expressions.at(0) = std::move(current);
        bt->variant = std::move(bexpr);
        current = bt.keep();
      }
    }
  }
  return current;
}

Handle<SyntaxTree::Expression>
Parser::logical_or_expression()
{
  auto current = logical_and_expression();

  if (current)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_LOGIC_OR)
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto bexpr = tree.add<SyntaxTree::BinaryExpression>();
      bexpr->type = tokenizer.next();
      bexpr->expressions.at(1) = logical_and_expression();
      if (bexpr->expressions.at(1))
      {
        bexpr->expressions.at(0) = std::move(current);
        bt->variant = std::move(bexpr);
        current = bt.keep();
      }
    }
  }
  return current;
}

Handle<SyntaxTree::Expression>
Parser::conditional_expression()
{
  if (auto handle = logical_or_expression())
  {
    if (tokenizer.peek<0>().type == Token::Type::PUNCT_QUESTION_MARK)
    {
      auto bt = backtracker(tree.add<SyntaxTree::Expression>());
      auto cexpr = tree.add<SyntaxTree::ConditionalExpression>();

      tokenizer.next();
      cexpr->expressions.at(1) = expression();
      if (cexpr->expressions.at(1) &&
        (tokenizer.peek<0>().type == Token::Type::PUNCT_COLON))
      {
        tokenizer.next();
        cexpr->expressions.at(2) = conditional_expression();
        if (cexpr->expressions.at(2))
        {
          cexpr->expressions.at(0) = std::move(handle);
          bt->variant = std::move(cexpr);
          return bt.keep();
        }
      }
    }
    return handle;
  }
  return {};
}

Handle<SyntaxTree::Expression>
Parser::assignment_expression()
{
  Token backtrack_token = tokenizer.backtrack_token();
  auto left = unary_expression();
  if (left && is_assignment_operator(tokenizer.peek<0>()))
  {
    tokenizer.next();
    auto bt = backtracker(tree.add<SyntaxTree::Expression>());
    auto handle = tree.add<SyntaxTree::AssignmentExpression>(bt->variant);

    handle->expressions.at(0) = std::move(left);
    handle->expressions.at(1) = assignment_expression();
    if (handle->expressions.at(1))
    {
      bt->variant = std::move(handle);
      return bt.keep();
    }
    else
    {
      throw_error("Expected assignment-expression.", tokenizer.next());
    }
  }
  else
  {
    tokenizer.backtrack(backtrack_token);
    tree.remove(left);
    if (auto handle = conditional_expression())
    {
      return handle;
    }
  }
  return {};
}

Handle<SyntaxTree::Expression>
Parser::expression()
{
  auto first = assignment_expression();
  if (first)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_COMMA)
    {
      tokenizer.next();
      auto& eol = end_of_list(first);
      eol->next = assignment_expression();
      if (!eol->next)
      {
        throw_error("Expected assignment-expression.",
          tokenizer.next());
      }
    }
  }
  return first;
}

Handle<SyntaxTree::ConstantExpression>
Parser::constant_expression()
{
  auto bt = backtracker(tree.add<SyntaxTree::ConstantExpression>());

  if (auto handle = conditional_expression())
  {
    bt->expression = std::move(handle);
    return bt.keep();
  }

  return {};
}

Handle<SyntaxTree::Declaration>
Parser::declaration()
{
  auto bt = backtracker(tree.add<SyntaxTree::Declaration>());

  auto handle = declaration_specifiers();
  if (handle)
  {
    bt->declaration_specifiers = std::move(handle);
    bt->init_declarator_list = init_declarator_list();
    if (tokenizer.next().type == Token::Type::PUNCT_SEMICOLON)
    {
      return bt.keep();
    }
  }

  return {};
}

Handle<SyntaxTree::DeclarationSpecifiers>
Parser::declaration_specifiers()
{
  auto bt = backtracker(tree.add<SyntaxTree::DeclarationSpecifiers>());

  while (is_declaration_specifier(tokenizer.peek<0>()))
  {
    switch (tokenizer.peek<0>().type)
    {
      case Token::Type::KEYWD_AUTO:
      case Token::Type::KEYWD_REGISTER:
      case Token::Type::KEYWD_STATIC:
      case Token::Type::KEYWD_EXTERN:
        set_once(bt->storage_class.token, tokenizer.next(),
          "Storage class already set.");
        break;

      case Token::Type::KEYWD_CONST:
        set_once(bt->qualifiers.const_token, tokenizer.next(),
          "Const qualifier already set.");
        break;

      case Token::Type::KEYWD_VOLATILE:
        set_once(bt->qualifiers.volatile_token, tokenizer.next(),
          "Volatile qualifier already set.");
        break;

      case Token::Type::KEYWD_UNSIGNED:
      case Token::Type::KEYWD_SIGNED:
        {
          auto handle = tree.add<SyntaxTree::ArithmeticType>();
          handle->signed_token = tokenizer.next();
          switch (tokenizer.peek<0>().type)
          {
            case Token::Type::KEYWD_VOID:
            case Token::Type::KEYWD_CHAR:
            case Token::Type::KEYWD_SHORT:
            case Token::Type::KEYWD_INT:
            case Token::Type::KEYWD_LONG:
              handle->type_token = tokenizer.next();
              break;

            default:
              break;
          }
          set_once(bt->variant, std::move(handle),
            "ArithmeticType already set.");
        }
        break;

      case Token::Type::KEYWD_VOID:
      case Token::Type::KEYWD_CHAR:
      case Token::Type::KEYWD_SHORT:
      case Token::Type::KEYWD_INT:
      case Token::Type::KEYWD_LONG:
      case Token::Type::KEYWD_FLOAT:
      case Token::Type::KEYWD_DOUBLE:
        {
          auto handle = tree.add<SyntaxTree::ArithmeticType>();
          handle->type_token = tokenizer.next();
          set_once(bt->variant, std::move(handle),
            "ArithmeticType already set.");
        }
        break;

      case Token::Type::KEYWD_STRUCT:
      case Token::Type::KEYWD_UNION:
        bt->variant = struct_or_union_specifier();
        break;
      case Token::Type::KEYWD_ENUM:
        bt->variant = enum_specifier();
        break;

      // TODO: case for typedef-name

      default:
        break;
    }
  }

  if (std::visit([](auto&& handle) -> bool { return handle; }, bt->variant))
  {
    return bt.keep();
  }
  return {};
}

Handle<SyntaxTree::InitDeclarator>
Parser::init_declarator_list()
{
  auto first = init_declarator();
  if (first)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_COMMA)
    {
      tokenizer.next();
      auto& eol = end_of_list(first);
      eol->next = init_declarator();
      if (!eol->next)
      {
        throw_error("Expected init-declarator.",
          tokenizer.next());
      }
    }
  }
  return first;
}

Handle<SyntaxTree::InitDeclarator>
Parser::init_declarator()
{
  auto bt = backtracker(tree.add<SyntaxTree::InitDeclarator>());

  bt->declarator = declarator();
  if (bt->declarator)
  {
    if (tokenizer.peek<0>().type == Token::Type::PUNCT_ASSIGN)
    {
      tokenizer.next();
      bt->initializer = initializer();
    }
    return bt.keep();
  }
  return {};
}

Handle<SyntaxTree::StructUnion>
Parser::struct_or_union_specifier()
{
  switch (tokenizer.peek<0>().type)
  {
    case Token::Type::KEYWD_STRUCT:
    case Token::Type::KEYWD_UNION:
      {
        auto bt = backtracker(tree.add<SyntaxTree::StructUnion>());
        bt->type = tokenizer.next();
        if (tokenizer.peek<0>().type == Token::Type::IDENTIFIER)
        {
          bt->identifier = SyntaxTree::Identifier{tokenizer.next()};
        }
        if (tokenizer.peek<0>().type == Token::Type::PUNCT_START_BLOCK)
        {
          tokenizer.next();
          bt->declaration = struct_declaration_list();
          if (!bt->declaration)
          {
            throw_error("Expected struct-declaration-list.", tokenizer.next());
          }
          else if (tokenizer.peek<0>().type == Token::Type::PUNCT_END_BLOCK)
          {
            tokenizer.next();
            return bt.keep();
          }
          else
          {
            throw_error("Expected '}'.", tokenizer.next());
          }
        }
        else if (bt->identifier.token)
        {
          return bt.keep();
        }
      }
      break;

    default:
      break;
  }
  return {};
}

Handle<SyntaxTree::StructDeclaration>
Parser::struct_declaration_list()
{
  auto first = struct_declaration();
  auto* current = &first;
  while (*current)
  {
    auto handle = struct_declaration();
    auto& eol = end_of_list(first);
    eol->next = std::move(handle);
    current = &eol->next;
  }
  return first;
}

Handle<SyntaxTree::StructDeclaration>
Parser::struct_declaration()
{
  auto bt = backtracker(tree.add<SyntaxTree::StructDeclaration>());

  bt->declaration_specifiers = specifier_qualifier_list();
  bt->struct_declarator = struct_declarator_list();
  if (bt->declaration_specifiers && bt->struct_declarator)
  {
    if (tokenizer.peek<0>().type == Token::Type::PUNCT_SEMICOLON)
    {
      tokenizer.next();
      return bt.keep();
    }
    else
    {
      throw_error("Expected ';'.", tokenizer.next());
    }
  }
  return {};
}

Handle<SyntaxTree::DeclarationSpecifiers>
Parser::specifier_qualifier_list()
{
  auto bt = backtracker(tree.add<SyntaxTree::DeclarationSpecifiers>());

  while (is_declaration_specifier(tokenizer.peek<0>()))
  {
    switch (tokenizer.peek<0>().type)
    {
      case Token::Type::KEYWD_CONST:
        set_once(bt->qualifiers.const_token, tokenizer.next(),
          "Const qualifier already set.");
        break;

      case Token::Type::KEYWD_VOLATILE:
        set_once(bt->qualifiers.volatile_token, tokenizer.next(),
          "Volatile qualifier already set.");
        break;

      case Token::Type::KEYWD_UNSIGNED:
      case Token::Type::KEYWD_SIGNED:
        {
          auto handle = tree.add<SyntaxTree::ArithmeticType>();
          handle->signed_token = tokenizer.next();
          switch (tokenizer.peek<0>().type)
          {
            case Token::Type::KEYWD_VOID:
            case Token::Type::KEYWD_CHAR:
            case Token::Type::KEYWD_SHORT:
            case Token::Type::KEYWD_INT:
            case Token::Type::KEYWD_LONG:
              handle->type_token = tokenizer.next();
              break;

            default:
              break;
          }
          set_once(bt->variant, std::move(handle),
            "ArithmeticType already set.");
        }
        break;

      case Token::Type::KEYWD_VOID:
      case Token::Type::KEYWD_CHAR:
      case Token::Type::KEYWD_SHORT:
      case Token::Type::KEYWD_INT:
      case Token::Type::KEYWD_LONG:
      case Token::Type::KEYWD_FLOAT:
      case Token::Type::KEYWD_DOUBLE:
        {
          auto handle = tree.add<SyntaxTree::ArithmeticType>();
          handle->type_token = tokenizer.next();
          set_once(bt->variant, std::move(handle),
            "ArithmeticType already set.");
        }
        break;

      case Token::Type::KEYWD_STRUCT:
      case Token::Type::KEYWD_UNION:
        bt->variant = struct_or_union_specifier();
        break;

      case Token::Type::KEYWD_ENUM:
        bt->variant = enum_specifier();
        break;

      // TODO: case for typedef-name

      default:
        break;
    }
  }

  if (std::visit([](auto&& handle) -> bool { return handle; }, bt->variant))
  {
    return bt.keep();
  }
  return {};
}

Handle<SyntaxTree::StructDeclarator>
Parser::struct_declarator_list()
{
  auto first = struct_declarator();
  if (first)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_COMMA)
    {
      tokenizer.next();
      auto& eol = end_of_list(first);
      eol->next = struct_declarator();
      if (!eol->next)
      {
        throw_error("Expected struct-declarator.", tokenizer.next());
      }
    }
  }
  return first;
}

Handle<SyntaxTree::StructDeclarator>
Parser::struct_declarator()
{
  auto bt = backtracker(tree.add<SyntaxTree::StructDeclarator>());

  bt->declarator = declarator();
  if (tokenizer.peek<0>().type == Token::Type::PUNCT_COLON)
  {
    tokenizer.next();
    bt->constant_expression = constant_expression();
    if (!bt->constant_expression)
    {
      throw_error("Expected constant-expression.", tokenizer.next());
    }
    else
    {
      return bt.keep();
    }
  }
  else if (bt->declarator)
  {
    return bt.keep();
  }
  return {};
}

Handle<SyntaxTree::Enum>
Parser::enum_specifier()
{
  if (tokenizer.peek<0>().type == Token::Type::KEYWD_ENUM)
  {
    auto bt = backtracker(tree.add<SyntaxTree::Enum>());

    bt->type = tokenizer.next();
    if (tokenizer.peek<0>().type == Token::Type::IDENTIFIER)
    {
      bt->identifier = SyntaxTree::Identifier{tokenizer.next()};
    }
    if (tokenizer.peek<0>().type == Token::Type::PUNCT_START_BLOCK)
    {
      tokenizer.next();
      bt->enumerator = enumerator_list();
      if (!bt->enumerator)
      {
        throw_error("Expected enumerator-list.", tokenizer.next());
      }
      else if (tokenizer.peek<0>().type == Token::Type::PUNCT_END_BLOCK)
      {
        tokenizer.next();
        return bt.keep();
      }
      else
      {
        throw_error("Expected '}'.", tokenizer.next());
      }
    }
    else if (bt->identifier.token)
    {
      return bt.keep();
    }
  }
  return {};
}

Handle<SyntaxTree::Enumerator>
Parser::enumerator_list()
{
  auto first = enumerator();
  if (first)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_COMMA)
    {
      tokenizer.next();
      auto& eol = end_of_list(first);
      eol->next = enumerator();
      if (!eol->next)
      {
        throw_error("Expected enumerator.", tokenizer.next());
      }
    }
  }
  return first;
}

Handle<SyntaxTree::Enumerator>
Parser::enumerator()
{
  if (tokenizer.peek<0>().type == Token::Type::IDENTIFIER)
  {
    auto handle = tree.add<SyntaxTree::Enumerator>();
    handle->identifier = SyntaxTree::Identifier{tokenizer.next()};
    if (tokenizer.peek<0>().type == Token::Type::PUNCT_EQUAL)
    {
      tokenizer.next();
      handle->constant_expression = constant_expression();
      if (!handle->constant_expression)
      {
        throw_error("Expected constant-expression.", tokenizer.next());
      }
    }
    return handle;
  }
  return {};
}

Handle<SyntaxTree::Declarator>
Parser::declarator()
{
  auto bt = backtracker(tree.add<SyntaxTree::Declarator>());

  bt->pointer = pointer();
  bt->direct_declarator = direct_declarator();

  if (bt->direct_declarator)
  {
    return bt.keep();
  }

  return {};
}

Handle<SyntaxTree::DirectDeclarator>
Parser::direct_declarator()
{
  auto bt = backtracker(tree.add<SyntaxTree::DirectDeclarator>());

  switch (tokenizer.peek<0>().type)
  {
    case Token::Type::IDENTIFIER:
      bt->variant = SyntaxTree::Identifier{tokenizer.next()};
      break;

    case Token::Type::PUNCT_LEFT_PARENS:
      tokenizer.next();
      bt->variant = declarator();
      if (std::visit([](auto&& variant) -> bool
      {
        if constexpr (std::is_same_v<SyntaxTree::Identifier,
          std::remove_reference_t<decltype(variant)>>)
        {
          return variant.token;
        }
        else
        {
          return variant;
        }
      }, bt->variant))
      {
        throw_error("Expected declarator.", tokenizer.next());
      }
      else if (tokenizer.peek<0>().type !=
        Token::Type::PUNCT_RIGHT_PARENS)
      {
        throw_error("Expected ')'.", tokenizer.next());
      }
      else
      {
        tokenizer.next();
      }
      break;

    default:
      return {};
  }
  auto handle = bt.keep();
  while ((tokenizer.peek<0>().type == Token::Type::PUNCT_LEFT_BRACKETS)
    || (tokenizer.peek<0>().type == Token::Type::PUNCT_LEFT_PARENS))
  {
    switch (tokenizer.next().type)
    {
      case Token::Type::PUNCT_LEFT_BRACKETS:
        {
          auto bt2 = backtracker(
            tree.add<SyntaxTree::DirectDeclarator>());
          auto handle2 = tree.add<SyntaxTree::ArrayDeclarator>();

          handle2->constant_expression = constant_expression();
          if (!handle2)
          {
            throw_error("Expected constant-expression.",
              tokenizer.next());
          }
          if (tokenizer.peek<0>().type !=
            Token::Type::PUNCT_RIGHT_BRACKETS)
          {
            throw_error("Expected ']'.", tokenizer.next());
          }
          else
          {
            tokenizer.next();
            handle2->direct_declarator = std::move(handle);
            bt2->variant = std::move(handle2);
            handle = bt2.keep();
          }
        }
        break;

      case Token::Type::PUNCT_LEFT_PARENS:
        {
          auto bt2 = backtracker(
            tree.add<SyntaxTree::DirectDeclarator>());
          auto handle2 = tree.add<SyntaxTree::FunctionDeclarator>();
          handle2->parameter_declaration = parameter_type_list();
          if (tokenizer.peek<0>().type !=
            Token::Type::PUNCT_RIGHT_PARENS)
          {
            throw_error("Expected ')'.", tokenizer.next());
          }
          else
          {
            tokenizer.next();
            handle2->direct_declarator = std::move(handle);
            bt2->variant = std::move(handle2);
            handle = bt2.keep();
          }
        }
        break;

      default:
        // impossible
        break;
    }
  }
  return handle;
}

Handle<SyntaxTree::Pointer>
Parser::pointer()
{
  if (tokenizer.peek<0>().type == Token::Type::PUNCT_STAR)
  {
    tokenizer.next();
    auto handle = tree.add<SyntaxTree::Pointer>();
    handle->qualifiers = type_qualifier_list();
    handle->pointer = pointer();
    return handle;
  }
  return {};
}

SyntaxTree::Qualifiers
Parser::type_qualifier_list()
{
  SyntaxTree::Qualifiers result;

  while (is_type_qualifier(tokenizer.peek<0>()))
  {
    switch (tokenizer.peek<0>().type)
    {
      case Token::Type::KEYWD_CONST:
        set_once(result.const_token, tokenizer.next(),
          "Const qualifier already set.");
        break;

      case Token::Type::KEYWD_VOLATILE:
        set_once(result.volatile_token, tokenizer.next(),
          "Volatile qualifier already set.");
        break;

      default:
        break;
    }
  }

  return result;
}

Handle<SyntaxTree::ParameterDeclaration>
Parser::parameter_type_list()
{
  auto handle = parameter_list();

  if (handle)
  {
    if ((tokenizer.peek<0>().type == Token::Type::PUNCT_COMMA)
      && (tokenizer.peek<1>().type == Token::Type::PUNCT_ELLIPSIS))
    {
      tokenizer.next();
      tokenizer.next();
      end_of_list(handle)->next =
        tree.add<SyntaxTree::ParameterDeclaration>();
    }
  }
  return handle;
}

Handle<SyntaxTree::ParameterDeclaration>
Parser::parameter_list()
{
  auto first = parameter_declaration();
  if (first)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_COMMA)
    {
      tokenizer.next();
      auto& eol = end_of_list(first);
      eol->next = parameter_declaration();
      if (!eol->next)
      {
        throw_error("Expected parameter-declaration.",
          tokenizer.next());
      }
    }
  }
  return first;
}

Handle<SyntaxTree::ParameterDeclaration>
Parser::parameter_declaration()
{
  auto bt = backtracker(tree.add<SyntaxTree::ParameterDeclaration>());

  bt->declaration_specifiers = declaration_specifiers();
  if (!bt->declaration_specifiers)
  {
    return {};
  }
  bt->declarator = declarator();
  if (!bt->declarator)
  {
    bt->declarator = abstract_declarator();
  }

  return bt.keep();
}

Handle<SyntaxTree::TypeName>
Parser::type_name()
{
  auto bt = backtracker(tree.add<SyntaxTree::TypeName>());

  if (auto handle = specifier_qualifier_list())
  {
    bt->declaration_specifiers = std::move(handle);
    bt->declarator = abstract_declarator();
    return bt.keep();
  }
  return {};
}

Handle<SyntaxTree::Declarator>
Parser::abstract_declarator()
{
  auto bt = backtracker(tree.add<SyntaxTree::Declarator>());

  bt->pointer = pointer();
  bt->direct_declarator = direct_abstract_declarator();
  if (bt->pointer || bt->direct_declarator)
  {
    return bt.keep();
  }
  return {};
}

Handle<SyntaxTree::DirectDeclarator>
Parser::direct_abstract_declarator()
{
  auto bt = backtracker(tree.add<SyntaxTree::DirectDeclarator>());

  switch (tokenizer.peek<0>().type)
  {
    case Token::Type::PUNCT_LEFT_PARENS:
      tokenizer.next();
      bt->variant = declarator();
      if (std::visit([](auto&& variant) -> bool
      {
        if constexpr (std::is_same_v<SyntaxTree::Identifier,
          std::remove_reference_t<decltype(variant)>>)
        {
          return variant.token;
        }
        else
        {
          return variant;
        }
      }, bt->variant))
      {
        throw_error("Expected declarator.", tokenizer.next());
      }
      else if (tokenizer.peek<0>().type !=
        Token::Type::PUNCT_RIGHT_PARENS)
      {
        throw_error("Expected ')'.", tokenizer.next());
      }
      else
      {
        tokenizer.next();
      }
      break;

    default:
      return {};
  }
  auto handle = bt.keep();
  while ((tokenizer.peek<0>().type == Token::Type::PUNCT_LEFT_BRACKETS)
    || (tokenizer.peek<0>().type == Token::Type::PUNCT_LEFT_PARENS))
  {
    switch (tokenizer.next().type)
    {
      case Token::Type::PUNCT_LEFT_BRACKETS:
        {
          auto bt2 = backtracker(
            tree.add<SyntaxTree::DirectDeclarator>());
          auto handle2 = tree.add<SyntaxTree::ArrayDeclarator>();

          handle2->constant_expression = constant_expression();
          if (!handle2)
          {
            throw_error("Expected constant-expression.",
              tokenizer.next());
          }
          if (tokenizer.peek<0>().type !=
            Token::Type::PUNCT_RIGHT_BRACKETS)
          {
            throw_error("Expected ']'.", tokenizer.next());
          }
          else
          {
            tokenizer.next();
            handle2->direct_declarator = std::move(handle);
            bt2->variant = std::move(handle2);
            handle = bt2.keep();
          }
        }
        break;

      case Token::Type::PUNCT_LEFT_PARENS:
        {
          auto bt2 = backtracker(
            tree.add<SyntaxTree::DirectDeclarator>());
          auto handle2 = tree.add<SyntaxTree::FunctionDeclarator>();
          handle2->parameter_declaration = parameter_type_list();
          if (tokenizer.peek<0>().type !=
            Token::Type::PUNCT_RIGHT_PARENS)
          {
            throw_error("Expected ')'.", tokenizer.next());
          }
          else
          {
            tokenizer.next();
            handle2->direct_declarator = std::move(handle);
            bt2->variant = std::move(handle2);
            handle = bt2.keep();
          }
        }
        break;

      default:
        // impossible
        break;
    }
  }
  return handle;
}

Handle<SyntaxTree::Initializer>
Parser::initializer()
{
  auto bt = backtracker(tree.add<SyntaxTree::Initializer>());

  if (tokenizer.peek<0>().type == Token::Type::PUNCT_START_BLOCK)
  {
    auto handle = initializer_list();
    if (handle)
    {
      bt->variant = std::move(handle);
      if (tokenizer.peek<0>().type == Token::Type::PUNCT_COMMA)
      {
        tokenizer.next();
      }
      if (tokenizer.peek<0>().type == Token::Type::PUNCT_END_BLOCK)
      {
        tokenizer.next();
      }
      else
      {
        throw_error("Expected '}'.", tokenizer.next());
      }
      return bt.keep();
    }
  }
  else
  {
    auto handle = assignment_expression();
    if (handle)
    {
      bt->variant = std::move(handle);
      return bt.keep();
    }
  }
  return {};
}

Handle<SyntaxTree::InitializerList>
Parser::initializer_list()
{
  auto bt = backtracker(tree.add<SyntaxTree::InitializerList>());
  bt->initializer = initializer();
  auto first = bt.keep();
  if (first)
  {
    while (tokenizer.peek<0>().type == Token::Type::PUNCT_COMMA)
    {
      tokenizer.next();
      auto bt = backtracker(tree.add<SyntaxTree::InitializerList>());
      bt->initializer = initializer();
      if (!bt->initializer)
      {
        throw_error("Expected initializer.", tokenizer.next());
      }
      auto& eol = end_of_list(first);
      eol->next = bt.keep();
    }
    return bt.keep();
  }
  return {};

}

Handle<SyntaxTree::Statement>
Parser::statement()
{
  if (auto handle = labeled_statement())
  {
    return handle;
  }
  if (auto handle = compound_statement())
  {
    auto parent = tree.add<SyntaxTree::Statement>();
    parent->variant = std::move(handle);
    return parent;
  }
  if (auto handle = expression_statement())
  {
    auto parent = tree.add<SyntaxTree::Statement>();
    parent->variant = std::move(handle);
    return parent;
  }
  if (auto handle = selection_statement())
  {
    return handle;
  }
  if (auto handle = iteration_statement())
  {
    return handle;
  }
  if (auto handle = jump_statement())
  {
    return handle;
  }

  return {};
}

Handle<SyntaxTree::Statement>
Parser::labeled_statement()
{
  switch (tokenizer.peek<0>().type)
  {
    case Token::Type::IDENTIFIER:
      if (tokenizer.peek<1>().type == Token::Type::PUNCT_COLON)
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::GotoTarget>();
        handle->identifier = SyntaxTree::Identifier{tokenizer.next()};
        tokenizer.next();
        handle->statement = statement();

        if (handle->statement)
        {
          bt->variant = std::move(handle);
          return bt.keep();
        }
      }
      break;

    case Token::Type::KEYWD_CASE:
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::CaseTarget>();
        tokenizer.next();
        handle->constant_expression = constant_expression();
        if (handle->constant_expression &&
          (tokenizer.next().type == Token::Type::PUNCT_COLON))
        {
          handle->statement = statement();
          if (handle->statement)
          {
            bt->variant = std::move(handle);
            return bt.keep();
          }
        }
      }
      break;

    case Token::Type::KEYWD_DEFAULT:
      if (tokenizer.peek<1>().type == Token::Type::PUNCT_COLON)
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::DefaultTarget>();
        tokenizer.next();
        tokenizer.next();
        handle->statement = statement();
        if (handle->statement)
        {
          bt->variant = std::move(handle);
          return bt.keep();
        }
      }
      break;

    default:
      break;
  }

  return {};
}

Handle<SyntaxTree::CompoundStatement>
Parser::compound_statement()
{
  auto bt = backtracker(tree.add<SyntaxTree::CompoundStatement>());

  if (tokenizer.next().type == Token::Type::PUNCT_START_BLOCK)
  {
    bt->declaration = declaration_list();
    bt->statement = statement_list();
    if (tokenizer.next().type == Token::Type::PUNCT_END_BLOCK)
    {
      return bt.keep();
    }
  }
  return {};
}

Handle<SyntaxTree::Declaration>
Parser::declaration_list()
{
  auto first = declaration();
  auto* current = &first;
  while (*current)
  {
    auto handle = declaration();
    auto& eol = end_of_list(first);
    eol->next = std::move(handle);
    current = &eol->next;
  }
  return first;
}

Handle<SyntaxTree::Statement>
Parser::statement_list()
{
  auto first = statement();
  auto* current = &first;
  while (*current)
  {
    auto handle = statement();
    auto& eol = end_of_list(first);
    eol->next = std::move(handle);
    current = &eol->next;
  }

  return first;
}

Handle<SyntaxTree::ExpressionStatement>
Parser::expression_statement()
{
  auto bt = backtracker(tree.add<SyntaxTree::ExpressionStatement>());

  bt->expression = expression();
  if (tokenizer.next().type == Token::Type::PUNCT_SEMICOLON)
  {
    return bt.keep();
  }

  return {};
}

Handle<SyntaxTree::Statement>
Parser::selection_statement()
{
  switch (tokenizer.peek<0>().type)
  {
    case Token::Type::KEYWD_IF:
      if (tokenizer.peek<1>().type == Token::Type::PUNCT_LEFT_PARENS)
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::IfStatement>();
        tokenizer.next();
        tokenizer.next();
        handle->expression = expression();
        if (handle->expression &&
          (tokenizer.next().type == Token::Type::PUNCT_RIGHT_PARENS))
        {
          handle->true_statement = statement();
          if (handle->true_statement)
          {
            if (tokenizer.next().type == Token::Type::KEYWD_ELSE)
            {
              handle->false_statement = statement();
            }
            bt->variant = std::move(handle);
            return bt.keep();
          }
        }
      }
      break;

    case Token::Type::KEYWD_SWITCH:
      if (tokenizer.peek<1>().type == Token::Type::PUNCT_LEFT_PARENS)
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::SwitchStatement>();
        tokenizer.next();
        tokenizer.next();
        handle->expression = expression();
        if (handle->expression &&
          (tokenizer.next().type == Token::Type::PUNCT_RIGHT_PARENS))
        {
          if (handle->statement = statement())
          {
            bt->variant = std::move(handle);
            return bt.keep();
          }
        }
      }
      break;

    default:
      break;
  }

  return {};
}

Handle<SyntaxTree::Statement>
Parser::iteration_statement()
{
  switch (tokenizer.peek<0>().type)
  {
    case Token::Type::KEYWD_WHILE:
      if (tokenizer.peek<1>().type == Token::Type::PUNCT_LEFT_PARENS)
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::WhileStatement>();
        tokenizer.next();
        tokenizer.next();
        handle->expression = expression();
        if (handle->expression &&
          (tokenizer.next().type == Token::Type::PUNCT_RIGHT_PARENS))
        {
          handle->statement = statement();
          if (handle->statement)
          {
            bt->variant = std::move(handle);
            return bt.keep();
          }
        }
      }
      break;

    case Token::Type::KEYWD_DO:
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::DoWhileStatement>();
        tokenizer.next();
        handle->statement = statement();
        if (handle->statement &&
          (tokenizer.next().type == Token::Type::KEYWD_WHILE) &&
          (tokenizer.next().type == Token::Type::PUNCT_LEFT_PARENS))
        {
          if ((handle->expression = expression()) &&
            (tokenizer.next().type == Token::Type::PUNCT_RIGHT_PARENS) &&
            (tokenizer.next().type == Token::Type::PUNCT_SEMICOLON))
          {
            bt->variant = std::move(handle);
            return bt.keep();
          }
        }
      }
      break;

    case Token::Type::KEYWD_FOR:
      if (tokenizer.peek<1>().type == Token::Type::PUNCT_LEFT_PARENS)
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::ForStatement>();
        tokenizer.next();
        tokenizer.next();
        handle->expressions.at(0) = expression();
        if (tokenizer.next().type == Token::Type::PUNCT_SEMICOLON)
        {
          handle->expressions.at(1) = expression();
          if (tokenizer.next().type == Token::Type::PUNCT_SEMICOLON)
          {
            handle->expressions.at(2) = expression();
            if (tokenizer.next().type ==
              Token::Type::PUNCT_RIGHT_PARENS)
            {
              if (handle->statement = statement())
              {
                bt->variant = std::move(handle);
                return bt.keep();
              }
            }
          }
        }
      }
      break;

    default:
      break;
  }

  return {};
}

Handle<SyntaxTree::Statement>
Parser::jump_statement()
{
  switch (tokenizer.peek<0>().type)
  {
    case Token::Type::KEYWD_GOTO:
      if (tokenizer.peek<1>().type == Token::Type::IDENTIFIER)
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::GotoStatement>();
        tokenizer.next();
        handle->identifier = SyntaxTree::Identifier{tokenizer.next()};
        if (tokenizer.next().type == Token::Type::PUNCT_SEMICOLON)
        {
          bt->variant = std::move(handle);
          return bt.keep();
        }
      }
      break;

    case Token::Type::KEYWD_CONTINUE:
      if (tokenizer.peek<1>().type == Token::Type::PUNCT_SEMICOLON)
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::ContinueStatement>();
        handle->token = tokenizer.next();
        tokenizer.next();
        bt->variant = std::move(handle);
        return bt.keep();
      }
      break;

    case Token::Type::KEYWD_BREAK:
      if (tokenizer.peek<1>().type == Token::Type::PUNCT_COLON)
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::BreakStatement>();
        handle->token = tokenizer.next();
        tokenizer.next();
        bt->variant = std::move(handle);
        return bt.keep();
      }
      break;

    case Token::Type::KEYWD_RETURN:
      {
        auto bt = backtracker(tree.add<SyntaxTree::Statement>());
        auto handle = tree.add<SyntaxTree::ReturnStatement>();
        tokenizer.next();
        handle->expression = expression();
        if (tokenizer.next().type == Token::Type::PUNCT_SEMICOLON)
        {
          bt->variant = std::move(handle);
          return bt.keep();
        }
      }
      break;

    default:
      break;
  }

  return {};

}

Handle<SyntaxTree::ExternalDeclaration>
Parser::translation_unit()
{
  auto first = external_declaration();
  if (first)
  {
    while (auto next = external_declaration())
    {
      end_of_list(first)->next = std::move(next);
    }
  }

  if (tokenizer.peek<0>().type != Token::Type::END_OF_FILE)
  {
    throw_error("Unexpected token.", tokenizer.next());
  }

  return first;
}

Handle<SyntaxTree::ExternalDeclaration>
Parser::external_declaration()
{
  auto bt = backtracker(tree.add<SyntaxTree::ExternalDeclaration>());

  if (auto handle = function_definition())
  {
    bt->variant = std::move(handle);
    return bt.keep();
  }

  if (auto handle = declaration())
  {
    bt->variant = std::move(handle);
    return bt.keep();
  }

  return {};
}

Handle<SyntaxTree::FunctionDefinition>
Parser::function_definition()
{
  auto bt = backtracker(tree.add<SyntaxTree::FunctionDefinition>());

  bt->declaration_specifiers = declaration_specifiers();
  bt->declarator = declarator();
  // K&R style declaration list not supported!
  bt->compound_statement = compound_statement();

  if (bt->declarator && bt->compound_statement)
  {
    return bt.keep();
  }
  return {};
}

DynamicSyntaxTree parse(const char* source)
{
  DynamicSyntaxTree tree;
  Parser parser(source, tree);

  return std::move(tree);
}

void parse(const char* source, SyntaxTree& tree)
{
  Parser parser(source, tree);
}

