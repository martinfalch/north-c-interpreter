  //| //|
 //||//|| Martin Falch, 2018.
// ||/ || GNU GPLv3

#pragma once

#include <stdexcept>
#include <variant>
#include <tuple>
#include <utility>

#include "tokenizer.h"

template <typename... Types>
struct TypeMap
{
    template <std::size_t N, typename T, typename Head, typename... Rest>
    static constexpr std::size_t search()
    {
        if constexpr (std::is_same_v<T, Head>)
        {
            return N;
        }
        else if (sizeof...(Rest) > 0)
        {
            return search<N+1, T, Rest...>();
        }
        else
        {
          static_assert("Type not in map!");
        }
    }

    template <typename T>
    static constexpr std::size_t id()
    {
        return search<0, T, Types...>();
    }
};

class SyntaxTree
{
  public:
  class Error : public std::runtime_error
  {
    using std::runtime_error::runtime_error;
  };

  using Size = unsigned int;
  using Offset = std::size_t;
  using TypeId = unsigned int;

  struct BaseHandle { }; // used for walk().

  template <typename T>
  struct Handle : BaseHandle
  {
    constexpr Handle()
      :
      tree(nullptr)
    { }

    constexpr Handle(Handle&& h)
      :
      tree(h.tree),
      offset(h.offset)
    {
      h.tree = nullptr;
    }

    constexpr Handle(SyntaxTree& tree, Offset offset)
      :
      tree(&tree),
      offset(offset)
    { }

    constexpr Handle(const Handle& h)
      :
      tree(h.tree),
      offset(h.offset)
    { }

    constexpr Handle& operator=(const Handle& h)
    {
      tree = h.tree;
      offset = h.offset;
      return *this;
    }

    constexpr operator bool() const
    {
      return tree != nullptr;
    }

    constexpr T& operator*()
    {
      if (tree == nullptr)
        throw Error("Uninitialized SyntaxTree::Handle.");
      return tree->get(*this);
    }

    constexpr const T& operator*() const
    {
      if (tree == nullptr)
        throw Error("Uninitialized SyntaxTree::Handle.");
      return tree->get(*this);
    }

    constexpr T* operator->()
    {
      if (tree == nullptr)
        throw Error("Uninitialized SyntaxTree::Handle.");
      return &tree->get(*this);
    }

    constexpr const T* operator->() const
    {
      if (tree == nullptr)
        throw Error("Uninitialized SyntaxTree::Handle.");
      return &tree->get(*this);
    }

    Offset index() const
    {
      if (tree == nullptr)
        throw Error("Uninitialized SyntaxTree::Handle.");
      return offset;
    }

    SyntaxTree* tree;
    Offset offset;
  };

  struct Identifier
  {
    Token token;
  };

  struct PrimaryExpression
  {
    Token token; // identifier, constant or string literal.
  };

  struct DeclarationSpecifiers; // TODO: optimize ordering
  struct Declarator;
  struct TypeName
  {
    Handle<DeclarationSpecifiers> declaration_specifiers;
    Handle<Declarator> declarator;
  };

  struct Expression;
  struct UnaryExpression
  {
    Token type;
    std::variant<
      Handle<TypeName>,
      Handle<Expression>> variant;
  };

  struct ArraySubscriptExpression
  {
    std::array<Handle<Expression>, 2> expressions;
  };

  struct FunctionCallExpression
  {
    std::array<Handle<Expression>, 2> expressions;
  };

  struct MemberAccessExpression
  {
    Token type;
    Handle<Expression> expression;
    Identifier identifier;
  };

  struct PostfixExpression
  {
    Token type;
    Handle<Expression> expression;
  };

  struct DeclarationSpecifiers;
  struct Declarator;
  struct CastExpression
  {
    Handle<TypeName> type_name;
    Handle<Expression> expression;
  };

  struct BinaryExpression
  {
    Token type;
    std::array<Handle<Expression>, 2> expressions;
  };

  struct ConditionalExpression
  {
    std::array<Handle<Expression>, 3> expressions;
  };

  struct AssignmentExpression
  {
    Token type;
    std::array<Handle<Expression>, 2> expressions;
  };

  struct Expression
  {
    std::variant<
      Handle<PrimaryExpression>,
      Handle<UnaryExpression>,
      Handle<ArraySubscriptExpression>,
      Handle<FunctionCallExpression>,
      Handle<MemberAccessExpression>,
      Handle<PostfixExpression>,
      Handle<CastExpression>,
      Handle<BinaryExpression>,
      Handle<ConditionalExpression>,
      Handle<AssignmentExpression>> variant;
    Handle<Expression> next;
  };

  struct ConstantExpression
  {
    Handle<Expression> expression;
  };

  struct StorageClass
  {
    Token token; // auto, register, static, extern.
  };

  struct Qualifiers
  {
    Token const_token;
    Token volatile_token;
  };

  struct ArithmeticType
  {
    Token signed_token;
    Token type_token;
  };

  struct StructDeclarator
  {
    Handle<Declarator> declarator;
    Handle<ConstantExpression> constant_expression;
    Handle<StructDeclarator> next;
  };

  struct StructDeclaration
  {
    Handle<DeclarationSpecifiers> declaration_specifiers;
    Handle<StructDeclarator> struct_declarator;
    Handle<StructDeclaration> next;
  };

  struct StructUnion
  {
    Token type;
    Identifier identifier;
    Handle<StructDeclaration> declaration;
  };

  struct Enumerator
  {
    Identifier identifier;
    Handle<ConstantExpression> constant_expression;
    Handle<Enumerator> next;
  };

  struct Enum
  {
    Token type;
    Identifier identifier;
    Handle<Enumerator> enumerator;
  };

  struct DeclarationSpecifiers
  {
    StorageClass storage_class;
    Qualifiers qualifiers;
    std::variant<
      Handle<ArithmeticType>,
      Handle<StructUnion>,
      Handle<Enum>> variant;
  };

  struct Pointer
  {
    Qualifiers qualifiers;
    Handle<Pointer> pointer;
  };

  struct DirectDeclarator;
  struct ArrayDeclarator
  {
    Handle<DirectDeclarator> direct_declarator;
    Handle<ConstantExpression> constant_expression;
  };

  struct Declarator;
  struct ParameterDeclaration
  {
    Handle<DeclarationSpecifiers> declaration_specifiers;
    Handle<Declarator> declarator;
    Handle<ParameterDeclaration> next;
  };

  struct FunctionDeclarator
  {
    Handle<DirectDeclarator> direct_declarator;
    Handle<ParameterDeclaration> parameter_declaration;
  };

  struct DirectDeclarator
  {
    std::variant<
      Identifier,
      Handle<Declarator>,
      Handle<ArrayDeclarator>,
      Handle<FunctionDeclarator>> variant;
  };

  struct Declarator
  {
    Handle<Pointer> pointer;
    Handle<DirectDeclarator> direct_declarator;
  };

  struct Initializer;
  struct InitializerList
  {
    Handle<Initializer> initializer;
    Handle<InitializerList> next;
  };

  struct Initializer
  {
    std::variant<
      Handle<Expression>,
      Handle<InitializerList>> variant;
  };

  struct Statement;
  struct GotoTarget
  {
    Identifier identifier;
    Handle<Statement> statement;
  };

  struct CaseTarget
  {
    Handle<ConstantExpression> constant_expression;
    Handle<Statement> statement;
  };

  struct DefaultTarget
  {
    Handle<Statement> statement;
  };

  struct ExpressionStatement
  {
    Handle<Expression> expression;
  };

  struct IfStatement
  {
    Handle<Expression> expression;
    Handle<Statement> true_statement;
    Handle<Statement> false_statement;
  };

  struct SwitchStatement
  {
    Handle<Expression> expression;
    Handle<Statement> statement;
  };

  struct WhileStatement
  {
    Handle<Expression> expression;
    Handle<Statement> statement;
  };

  struct DoWhileStatement
  {
    Handle<Statement> statement;
    Handle<Expression> expression;
  };

  struct ForStatement
  {
    std::array<Handle<Expression>, 3> expressions;
    Handle<Statement> statement;
  };

  struct GotoStatement
  {
    Identifier identifier;
  };

  struct ContinueStatement
  {
    Token token;
  };

  struct BreakStatement
  {
    Token token;
  };

  struct ReturnStatement
  {
    Handle<Expression> expression;
  };

  struct CompoundStatement;
  struct Statement
  {
    std::variant<
      Handle<GotoTarget>,               // + LabeledStatement
      Handle<CaseTarget>,               // |
      Handle<DefaultTarget>,            // |
      Handle<CompoundStatement>,        //
      Handle<ExpressionStatement>,      //
      Handle<IfStatement>,              // + SelectionStatement
      Handle<SwitchStatement>,          // |
      Handle<WhileStatement>,           // + IterationStatement
      Handle<DoWhileStatement>,         // |
      Handle<ForStatement>,             // |
      Handle<GotoStatement>,            // + JumpStatement
      Handle<ContinueStatement>,        // |
      Handle<BreakStatement>,           // |
      Handle<ReturnStatement>> variant; // |
    Handle<Statement> next;
  };

  struct Declaration;
  struct CompoundStatement
  {
    Handle<Declaration> declaration;
    Handle<Statement> statement;
  };

  struct InitDeclarator
  {
    Handle<Declarator> declarator;
    Handle<Initializer> initializer;
    Handle<InitDeclarator> next;
  };

  struct Declaration
  {
    Handle<DeclarationSpecifiers> declaration_specifiers;
    Handle<InitDeclarator> init_declarator_list;
    Handle<Declaration> next;
  };

  struct FunctionDefinition
  {
    Handle<DeclarationSpecifiers> declaration_specifiers;
    Handle<Declarator> declarator;
    Handle<CompoundStatement> compound_statement;
  };

  struct ExternalDeclaration
  {
    std::variant<
      Handle<Declaration>,
      Handle<FunctionDefinition>> variant;
    Handle<ExternalDeclaration> next;
  };

  using NodeMap = TypeMap<
    PrimaryExpression,
    TypeName,
    UnaryExpression,
    ArraySubscriptExpression,
    FunctionCallExpression,
    MemberAccessExpression,
    PostfixExpression,
    CastExpression,
    BinaryExpression,
    ConditionalExpression,
    AssignmentExpression,
    Expression,
    ConstantExpression,
    ArithmeticType,
    StructDeclarator,
    StructDeclaration,
    StructUnion,
    Enumerator,
    Enum,
    DeclarationSpecifiers,
    Pointer,
    ArrayDeclarator,
    ParameterDeclaration,
    FunctionDeclarator,
    DirectDeclarator,
    Declarator,
    InitializerList,
    Initializer,
    GotoTarget,
    CaseTarget,
    DefaultTarget,
    ExpressionStatement,
    IfStatement,
    SwitchStatement,
    WhileStatement,
    DoWhileStatement,
    ForStatement,
    GotoStatement,
    ContinueStatement,
    BreakStatement,
    ReturnStatement,
    Statement,
    CompoundStatement,
    InitDeclarator,
    Declaration,
    FunctionDefinition,
    ExternalDeclaration>;

  SyntaxTree(char* buffer)
    :
    buffer(buffer)
  { }

  SyntaxTree(SyntaxTree&& tree)
    :
    translation_unit(std::move(tree.translation_unit)),
    buffer(tree.buffer)
  {
    tree.buffer = nullptr;
  }

  virtual ~SyntaxTree() = default;

  virtual char* allocate(Size size, TypeId id, Offset& offset) = 0;
  virtual void deallocate(Offset offset) = 0;

  template <typename T>
  Handle<T> add()
  {
    TypeId id = static_cast<TypeId>(NodeMap::id<T>());
    Offset offset;

    new (allocate(sizeof(T), id, offset)) T;
    return Handle<T>(*this, offset);
  }

  template <typename T>
  Handle<T> add(Handle<T>& parent)
  {
    parent = add<T>();
    return parent;
  }

  template <typename T, typename... Args>
  Handle<T> add(std::variant<Args...>& parent)
  {
    parent = add<T>();
    return std::get<Handle<T>>(parent);
  }

  template <typename T>
  void remove(const Handle<T>& handle)
  {
    deallocate(handle.offset);
  }

  void destroy(TypeId id, char* memory)
  {
    switch (id)
    {
      case NodeMap::id<PrimaryExpression>():
        reinterpret_cast<PrimaryExpression*>(memory)->~PrimaryExpression();
        break;
      case NodeMap::id<TypeName>():
        reinterpret_cast<TypeName*>(memory)->~TypeName();
        break;
      case NodeMap::id<UnaryExpression>():
        reinterpret_cast<UnaryExpression*>(memory)->~UnaryExpression();
        break;
      case NodeMap::id<ArraySubscriptExpression>():
        reinterpret_cast<ArraySubscriptExpression*>(memory)
          ->~ArraySubscriptExpression();
        break;
      case NodeMap::id<FunctionCallExpression>():
        reinterpret_cast<FunctionCallExpression*>(memory)->~FunctionCallExpression();
        break;
      case NodeMap::id<MemberAccessExpression>():
        reinterpret_cast<MemberAccessExpression*>(memory)->~MemberAccessExpression();
        break;
      case NodeMap::id<PostfixExpression>():
        reinterpret_cast<PostfixExpression*>(memory)->~PostfixExpression();
        break;
      case NodeMap::id<CastExpression>():
        reinterpret_cast<CastExpression*>(memory)->~CastExpression();
        break;
      case NodeMap::id<BinaryExpression>():
        reinterpret_cast<BinaryExpression*>(memory)->~BinaryExpression();
        break;
      case NodeMap::id<ConditionalExpression>():
        reinterpret_cast<ConditionalExpression*>(memory)->~ConditionalExpression();
        break;
      case NodeMap::id<AssignmentExpression>():
        reinterpret_cast<AssignmentExpression*>(memory)->~AssignmentExpression();
        break;
      case NodeMap::id<Expression>():
        reinterpret_cast<Expression*>(memory)->~Expression();
        break;
      case NodeMap::id<ConstantExpression>():
        reinterpret_cast<ConstantExpression*>(memory)->~ConstantExpression();
        break;
      case NodeMap::id<ArithmeticType>():
        reinterpret_cast<ArithmeticType*>(memory)->~ArithmeticType();
      case NodeMap::id<StructDeclarator>():
        reinterpret_cast<StructDeclarator*>(memory)->~StructDeclarator();
        break;
      case NodeMap::id<StructDeclaration>():
        reinterpret_cast<StructDeclaration*>(memory)->~StructDeclaration();
        break;
      case NodeMap::id<StructUnion>():
        reinterpret_cast<StructUnion*>(memory)->~StructUnion();
        break;
      case NodeMap::id<Enumerator>():
        reinterpret_cast<Enumerator*>(memory)->~Enumerator();
        break;
      case NodeMap::id<Enum>():
        reinterpret_cast<Enum*>(memory)->~Enum();
        break;
      case NodeMap::id<DeclarationSpecifiers>():
        reinterpret_cast<DeclarationSpecifiers*>(memory)->~DeclarationSpecifiers();
        break;
      case NodeMap::id<Pointer>():
        reinterpret_cast<Pointer*>(memory)->~Pointer();
        break;
      case NodeMap::id<ArrayDeclarator>():
        reinterpret_cast<ArrayDeclarator*>(memory)->~ArrayDeclarator();
        break;
      case NodeMap::id<ParameterDeclaration>():
        reinterpret_cast<ParameterDeclaration*>(memory)->~ParameterDeclaration();
        break;
      case NodeMap::id<FunctionDeclarator>():
        reinterpret_cast<FunctionDeclarator*>(memory)->~FunctionDeclarator();
        break;
      case NodeMap::id<DirectDeclarator>():
        reinterpret_cast<DirectDeclarator*>(memory)->~DirectDeclarator();
        break;
      case NodeMap::id<Declarator>():
        reinterpret_cast<Declarator*>(memory)->~Declarator();
        break;
      case NodeMap::id<InitializerList>():
        reinterpret_cast<InitializerList*>(memory)->~InitializerList();
        break;
      case NodeMap::id<Initializer>():
        reinterpret_cast<Initializer*>(memory)->~Initializer();
        break;
      case NodeMap::id<GotoTarget>():
        reinterpret_cast<GotoTarget*>(memory)->~GotoTarget();
        break;
      case NodeMap::id<CaseTarget>():
        reinterpret_cast<CaseTarget*>(memory)->~CaseTarget();
        break;
      case NodeMap::id<DefaultTarget>():
        reinterpret_cast<DefaultTarget*>(memory)->~DefaultTarget();
        break;
      case NodeMap::id<ExpressionStatement>():
        reinterpret_cast<ExpressionStatement*>(memory)->~ExpressionStatement();
        break;
      case NodeMap::id<IfStatement>():
        reinterpret_cast<IfStatement*>(memory)->~IfStatement();
        break;
      case NodeMap::id<SwitchStatement>():
        reinterpret_cast<SwitchStatement*>(memory)->~SwitchStatement();
        break;
      case NodeMap::id<WhileStatement>():
        reinterpret_cast<WhileStatement*>(memory)->~WhileStatement();
        break;
      case NodeMap::id<DoWhileStatement>():
        reinterpret_cast<DoWhileStatement*>(memory)->~DoWhileStatement();
        break;
      case NodeMap::id<ForStatement>():
        reinterpret_cast<ForStatement*>(memory)->~ForStatement();
        break;
      case NodeMap::id<GotoStatement>():
        reinterpret_cast<GotoStatement*>(memory)->~GotoStatement();
        break;
      case NodeMap::id<ContinueStatement>():
        reinterpret_cast<ContinueStatement*>(memory)->~ContinueStatement();
        break;
      case NodeMap::id<BreakStatement>():
        reinterpret_cast<BreakStatement*>(memory)->~BreakStatement();
        break;
      case NodeMap::id<ReturnStatement>():
        reinterpret_cast<ReturnStatement*>(memory)->~ReturnStatement();
        break;
      case NodeMap::id<Statement>():
        reinterpret_cast<Statement*>(memory)->~Statement();
        break;
      case NodeMap::id<CompoundStatement>():
        reinterpret_cast<CompoundStatement*>(memory)->~CompoundStatement();
        break;
      case NodeMap::id<InitDeclarator>():
        reinterpret_cast<InitDeclarator*>(memory)->~InitDeclarator();
        break;
      case NodeMap::id<Declaration>():
        reinterpret_cast<Declaration*>(memory)->~Declaration();
        break;
      case NodeMap::id<FunctionDefinition>():
        reinterpret_cast<FunctionDefinition*>(memory)->~FunctionDefinition();
        break;
      case NodeMap::id<ExternalDeclaration>():
        reinterpret_cast<ExternalDeclaration*>(memory)->~ExternalDeclaration();
        break;
      default:
        throw Error("Unknown type id.");
        break;
    }
  }

  template <typename T>
  const T& get(const Handle<T>& handle) const
  {
    if (!handle) throw Error("Invalid node handle.");
    if (buffer == nullptr) throw Error("SyntaxTree::buffer moved.");

    return *reinterpret_cast<T*>(&buffer[handle.offset]);
  }

  template <typename T>
  T& get(const Handle<T>& handle)
  {
    if (!handle) throw Error("Invalid node handle.");
    if (buffer == nullptr) throw Error("SyntaxTree::buffer moved.");

    return *reinterpret_cast<T*>(&buffer[handle.offset]);
  }

  void clear()
  {
    remove(translation_unit);
  }

  Handle<ExternalDeclaration> translation_unit;

  protected:
  static constexpr Offset align(Offset offset)
  {
    return (offset + sizeof(int) - 1)/sizeof(int)*sizeof(int);
  }

  char* buffer;
};

template <typename T>
auto get_children(T& handle)
{
  if    constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::PrimaryExpression>>)
  {
    return std::forward_as_tuple();
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::TypeName>>)
  {
    return std::forward_as_tuple(handle->declaration_specifiers,
      handle->declarator);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::UnaryExpression>>)
  {
    return std::forward_as_tuple(handle->variant);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::ArraySubscriptExpression>>)
  {
    return std::forward_as_tuple(handle->expressions.at(0),
      handle->expressions.at(1));
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::FunctionCallExpression>>)
  {
    return std::forward_as_tuple(handle->expressions.at(0),
      handle->expressions.at(1));
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::MemberAccessExpression>>)
  {
    return std::forward_as_tuple(handle->expression);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::PostfixExpression>>)
  {
    return std::forward_as_tuple(handle->expression);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::CastExpression>>)
  {
    return std::forward_as_tuple(handle->type_name, handle->expression);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::BinaryExpression>>)
  {
    return std::forward_as_tuple(handle->expressions.at(0),
      handle->expressions.at(1));
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::ConditionalExpression>>)
  {
    return std::forward_as_tuple(handle->expressions.at(0),
      handle->expressions.at(1), handle->expressions.at(2));
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::AssignmentExpression>>)
  {
    return std::forward_as_tuple(handle->expressions.at(0),
      handle->expressions.at(1));
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::Expression>>)
  {
    return std::forward_as_tuple(handle->variant);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::ConstantExpression>>)
  {
    return std::forward_as_tuple(handle->expression);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::ArithmeticType>>)
  {
    return std::forward_as_tuple();
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::StructDeclarator>>)
  {
    return std::forward_as_tuple(handle->declarator,
      handle->constant_expression, handle->next);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::StructDeclaration>>)
  {
    return std::forward_as_tuple(handle->declaration_specifiers,
      handle->struct_declarator, handle->next);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::StructUnion>>)
  {
    return std::forward_as_tuple(handle->declaration);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::Enumerator>>)
  {
    return std::forward_as_tuple(handle->constant_expression,
      handle->next);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::Enum>>)
  {
    return std::forward_as_tuple(handle->enumerator);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::DeclarationSpecifiers>>)
  {
    return std::forward_as_tuple(handle->variant);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::Pointer>>)
  {
    return std::forward_as_tuple(handle->pointer);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::ArrayDeclarator>>)
  {
    return std::forward_as_tuple(handle->direct_declarator,
      handle->constant_expression);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::ParameterDeclaration>>)
  {
    return std::forward_as_tuple(handle->declaration_specifiers,
      handle->declarator, handle->next);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::FunctionDeclarator>>)
  {
    return std::forward_as_tuple(handle->direct_declarator,
      handle->parameter_declaration);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::DirectDeclarator>>)
  {
    return std::forward_as_tuple(handle->variant);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::Declarator>>)
  {
    return std::forward_as_tuple(handle->pointer,
      handle->direct_declarator);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::InitializerList>>)
  {
    return std::forward_as_tuple(handle->initializer, handle->next);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::Initializer>>)
  {
    return std::forward_as_tuple(handle->variant);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::GotoTarget>>)
  {
    return std::forward_as_tuple(handle->statement);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::CaseTarget>>)
  {
    return std::forward_as_tuple(handle->constant_expression,
      handle->statement);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::DefaultTarget>>)
  {
    return std::forward_as_tuple(handle->statement);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::ExpressionStatement>>)
  {
    return std::forward_as_tuple(handle->expression);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::IfStatement>>)
  {
    return std::forward_as_tuple(handle->expression,
      handle->true_statement, handle->false_statement);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::SwitchStatement>>)
  {
    return std::forward_as_tuple(handle->expression, handle->statement);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::WhileStatement>>)
  {
    return std::forward_as_tuple(handle->expression, handle->statement);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::DoWhileStatement>>)
  {
    return std::forward_as_tuple(handle->statement, handle->expression);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::ForStatement>>)
  {
    return std::forward_as_tuple(handle->expressions.at(0),
      handle->expressions.at(1), handle->expressions.at(2),
      handle->statement);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::GotoStatement>>)
  {
    return std::forward_as_tuple();
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::BreakStatement>>)
  {
    return std::forward_as_tuple();
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::ReturnStatement>>)
  {
    return std::forward_as_tuple(handle->expression);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::Statement>>)
  {
    return std::forward_as_tuple(handle->variant, handle->next);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::CompoundStatement>>)
  {
    return std::forward_as_tuple(handle->declaration, handle->statement);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::InitDeclarator>>)
  {
    return std::forward_as_tuple(handle->declarator, handle->initializer,
      handle->next);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::Declaration>>)
  {
    return std::forward_as_tuple(handle->declaration_specifiers,
      handle->init_declarator_list);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::FunctionDefinition>>)
  {
    return std::forward_as_tuple(handle->declaration_specifiers,
      handle->declarator, handle->compound_statement);
  }
  else if constexpr (std::is_same_v<T,
    SyntaxTree::Handle<SyntaxTree::ExternalDeclaration>>)
  {
    return std::forward_as_tuple(handle->variant, handle->next);
  }
  else
  {
    throw SyntaxTree::Error("Unknown type!");
    return std::forward_as_tuple();
  }
}

template <typename F0, typename F1, typename T>
void walk(F0&& pre, F1&& post, SyntaxTree::Handle<T>& handle)
{
  if (handle)
  {
    bool walk_children;
    if constexpr(std::is_same_v<void, decltype(pre(handle))>)
    {
      pre(handle);
      walk_children = true;
    }
    else
    {
      walk_children = pre(handle);
    }

    if (walk_children)
    {
      walk(std::forward<F0>(pre), std::forward<F1>(post),
        get_children(handle));
    }
    post(handle);
  }
}

template <typename F0, typename F1>
void walk(F0&&, F1&&, const Token&)
{
  // ignore
}

template <int N, typename F0, typename F1, typename... Args>
void walk_tuple(F0&& pre, F1&& post, const std::tuple<Args...>& tuple)
{
  if constexpr(N < sizeof...(Args))
  {
    walk(std::forward<F0>(pre), std::forward<F1>(post),
      std::get<N>(tuple));
    walk_tuple<N+1>(std::forward<F0>(pre), std::forward<F1>(post), tuple);
  }
}

template <typename F0, typename F1, typename... Args>
void walk(F0&& pre, F1&& post, const std::tuple<Args...>& tuple)
{
  walk_tuple<0>(std::forward<F0>(pre), std::forward<F1>(post), tuple);
}

template <typename F0, typename F1, typename... Args>
void walk(F0&& pre, F1&& post, std::variant<Args...>& variant)
{
  std::visit([&pre, &post](auto&& v)
  {
    if constexpr (std::is_base_of_v<
      SyntaxTree::BaseHandle,
      std::remove_reference_t<decltype(v)>>)
    {
      walk(std::forward<F0>(pre), std::forward<F1>(post), v);
    }
  }, variant);
}

template <typename T>
const char* get_name(const SyntaxTree::Handle<T>&)
{
  if    constexpr (std::is_same_v<T, SyntaxTree::PrimaryExpression>)
  {
    return "PrimExpr";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::UnaryExpression>)
  {
    return "UnaryExpr";
  }
  else if constexpr (
    std::is_same_v<T, SyntaxTree::ArraySubscriptExpression>)
  {
    return "ArrSbsExpr";
  }
  else if constexpr (
    std::is_same_v<T, SyntaxTree::FunctionCallExpression>)
  {
    return "FnCallExpr";
  }
  else if constexpr (
    std::is_same_v<T, SyntaxTree::MemberAccessExpression>)
  {
    return "MbrAccExpr";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::PostfixExpression>)
  {
    return "PstfxExpr";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::CastExpression>)
  {
    return "CastExpr";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::BinaryExpression>)
  {
    return "BinExpr";
  }
  else if constexpr (
    std::is_same_v<T, SyntaxTree::ConditionalExpression>)
  {
    return "CondExpr";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::AssignmentExpression>)
  {
    return "AssnExpr";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::Expression>)
  {
    return "Expr";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::ConstantExpression>)
  {
    return "ConstExpr";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::ArithmeticType>)
  {
    return "ArithType";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::StructDeclarator>)
  {
    return "StructDecl'or";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::StructDeclaration>)
  {
    return "StructDecl'on";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::StructUnion>)
  {
    return "StructUnion";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::Enum>)
  {
    return "Enum";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::DeclarationSpecifiers>)
  {
    return "DeclarationSpecifiers";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::Pointer>)
  {
    return "Pointer";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::ArrayDeclarator>)
  {
    return "ArrDecl'or";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::ParameterDeclaration>)
  {
    return "ParamDecl'ion";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::FunctionDeclarator>)
  {
    return "FnDecl'or";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::DirectDeclarator>)
  {
    return "DirDecl'or";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::Declarator>)
  {
    return "Decl'or";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::Initializer>)
  {
    return "Init";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::GotoTarget>)
  {
    return "GotoTgt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::CaseTarget>)
  {
    return "CaseTgt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::DefaultTarget>)
  {
    return "DefTgt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::ExpressionStatement>)
  {
    return "ExprStmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::IfStatement>)
  {
    return "IfStmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::SwitchStatement>)
  {
    return "SwitchStmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::WhileStatement>)
  {
    return "WhileStmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::DoWhileStatement>)
  {
    return "DoWhileStmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::ForStatement>)
  {
    return "ForStmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::GotoStatement>)
  {
    return "GotoStmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::BreakStatement>)
  {
    return "BreakStmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::ReturnStatement>)
  {
    return "RetStmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::Statement>)
  {
    return "Stmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::CompoundStatement>)
  {
    return "CompStmt";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::InitDeclarator>)
  {
    return "InitDecl'or";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::Declaration>)
  {
    return "Decl'ion";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::FunctionDefinition>)
  {
    return "FnDef";
  }
  else if constexpr (std::is_same_v<T, SyntaxTree::ExternalDeclaration>)
  {
    return "ExtDecl'ion";
  }
  else
  {
    return typeid(T).name();
  }
}

template <SyntaxTree::Offset MEMORY_SIZE>
class StaticSyntaxTree : public SyntaxTree
{
  public:
  template <SyntaxTree::Offset N>
  StaticSyntaxTree(StaticSyntaxTree<N>&& tree)
    :
    SyntaxTree(buffer),
    used(0),
    num_nodes(0)
  {
    static_assert(N <= MEMORY_SIZE);

    walk([this](auto&& handle)
    {
      auto copy = add<std::remove_reference_t<decltype(*handle)>>();
      *copy = *handle;
    }, [](auto&&) { }, tree.translation_unit);
    translation_unit = Handle<ExternalDeclaration>(*this, 0);
  }

  StaticSyntaxTree()
    :
    SyntaxTree(buffer),
    used(0),
    num_nodes(0)
  { }

  virtual char* allocate(Size size, TypeId id, Offset& offset) override
  {
    char* result = &buffer[used];
    offset = used;

    if (used + num_nodes*sizeof(IndexEntry) > MEMORY_SIZE)
    {
      throw Error("Out of memory.");
    }

    index(num_nodes) = IndexEntry{id, offset};

    used += size;
    num_nodes += 1;

    return result;
  }

  virtual void deallocate(Offset offset) override
  {
    bool found = false;
    Offset new_num_nodes = num_nodes;

    for (Offset node = 0; node < num_nodes; node++)
    {
      if (index(node).offset == offset)
      {
        found = true;
        new_num_nodes = node;
      }

      if (found)
      {
        IndexEntry& entry = index(node);
        destroy(entry.type_id, &buffer[entry.offset]);
      }
    }

    num_nodes = new_num_nodes;
  }

  ~StaticSyntaxTree()
  {
    for (Offset node = 0; node < num_nodes; node++)
    {
      IndexEntry& entry = index(node);
      destroy(entry.type_id, &buffer[entry.offset]);
    }
  }

  protected:
  template <Offset N>
  friend class StaticSyntaxTree;

  struct IndexEntry
  {
    TypeId type_id;
    Offset offset;
  };

  IndexEntry& index(Offset i)
  {
    return *reinterpret_cast<IndexEntry*>(
      &buffer[MEMORY_SIZE-(i+1)*sizeof(IndexEntry)]);
  }

  char buffer[MEMORY_SIZE];     // the index is located at the end of the
  Offset used;                 // memory block.
  Offset num_nodes;
};

template <std::size_t INITIAL_SIZE = 1024>
class FastDynamicSyntaxTree : public SyntaxTree
{
  public:
  FastDynamicSyntaxTree(FastDynamicSyntaxTree&& tree)
    :
    SyntaxTree(std::move(tree)),
    index(std::move(tree.index)),
    allocated(tree.allocated),
    used(tree.used)
  {
    walk([this](auto&& handle)
    {
      handle.tree = this;
    }, [](auto&&) { }, translation_unit);
  }

  FastDynamicSyntaxTree()
    :
    SyntaxTree(new char[INITIAL_SIZE]),
    allocated(INITIAL_SIZE),
    used(0)
  { }

  virtual char* allocate(Size size, TypeId id, Offset& offset) override
  {
    char* result = &buffer[used];

    offset = used;
    if (used > allocated)
    {
      allocated *= 2;
      char* new_buffer = new char[allocated];

      walk([this, &new_buffer](auto&& handle)
      {
        auto copy = reinterpret_cast<
          std::remove_reference_t<decltype(*handle)>*>(
          &new_buffer[handle.offset]);
        *copy = *handle;
      }, [](auto&&) { }, translation_unit);

      delete[] buffer;
      buffer = new_buffer;
    }

    used += size;
    index.push_back(IndexEntry{id, offset});

    return result;
  }

  virtual void deallocate(Offset offset) override
  {
    auto first = std::find_if(index.begin(), index.end(),
      [&offset](auto&& entry) -> bool
      {
        return entry.offset == offset;
      });

    std::for_each(first, index.end(), [this](auto&& entry)
    {
      char* memory = reinterpret_cast<char*>(entry.offset+align(1));

      destroy(entry.type_id, memory);
    });

    index.erase(first, index.end());
  }

  ~FastDynamicSyntaxTree()
  {
    std::for_each(index.begin(), index.end(), [this](auto&& entry)
    {
      char* memory = reinterpret_cast<char*>(entry.offset+align(1));

      destroy(entry.type_id, memory);
    });
    delete[] buffer;
    buffer = nullptr;
  }

  protected:
  struct IndexEntry
  {
    TypeId type_id;
    Offset offset;
  };

  std::vector<IndexEntry> index;
  Offset allocated;
  Offset used;
};

class DynamicSyntaxTree : public SyntaxTree
{
  public:
  DynamicSyntaxTree(DynamicSyntaxTree&& tree)
    :
    SyntaxTree(std::move(tree)),
    index(std::move(tree.index))
  {
    walk([this](auto&& handle)
    {
      handle.tree = this;
    }, [](auto&&) { }, translation_unit);
  }

  DynamicSyntaxTree()
    :
    SyntaxTree(reinterpret_cast<char*>(align(1))) // avoiding nullptr buffer:
  { }                                            // offset is addr-alignment.

  virtual char* allocate(Size size, TypeId id, Offset& offset) override
  {
    char* result = new char[size];
    offset = result - reinterpret_cast<char*>(align(1));

    index.push_back(IndexEntry{id, offset});

    return result;
  }

  virtual void deallocate(Offset offset) override
  {
    auto first = std::find_if(index.begin(), index.end(),
      [&offset](auto&& entry) -> bool
      {
        return entry.offset == offset;
      });

    std::for_each(first, index.end(), [this](auto&& entry)
    {
      char* memory = reinterpret_cast<char*>(entry.offset+align(1));

      destroy(entry.type_id, memory);
      delete[] reinterpret_cast<char*>(memory);
    });

    index.erase(first, index.end());
  }

  ~DynamicSyntaxTree()
  {
    std::for_each(index.begin(), index.end(), [this](auto&& entry)
    {
      char* memory = reinterpret_cast<char*>(entry.offset+align(1));

      destroy(entry.type_id, memory);
      delete[] reinterpret_cast<char*>(memory);
    });
    buffer = nullptr;
  }

  protected:
  struct IndexEntry
  {
    TypeId type_id;
    Offset offset;
  };

  std::vector<IndexEntry> index;
};

std::ostream& operator<<(std::ostream& o, const SyntaxTree& tree);

