  //| //|
 //||//|| Martin Falch, 2018.
// ||/ || GNU GPLv3

#include "syntaxtree.h"

template <int N, typename... Args>
void print_anchors(std::ostream& o, const std::tuple<Args...>& children)
{
  if constexpr (N < sizeof...(Args))
  {
    if constexpr (N == 0)
    {
      o << "|{<m0> ";
    }
    else
    {
      o << "|<m" << N << "> ";
    }
    print_anchors<N+1>(o, children);
  }
  else if constexpr (N > 0)
  {
    o << "}";
  }
}

template <typename T0, typename T1>
void print_edge(std::ostream& o, int level, SyntaxTree::Handle<T0>& parent,
  int index, SyntaxTree::Handle<T1>& child)
{
  if (child)
  {
    o << std::string((level+1)*2, ' ');
    o << "n" << parent.index() << ":m" << index;
    o << " -> n" << child.index() << ";\n";
  }
}

template <int N, typename T, typename... Args>
void print_edges(std::ostream& o, int level, SyntaxTree::Handle<T>& parent,
  const std::tuple<Args...>& children)
{
  (void)level; // TODO: why does removing this generate warnings?

  if constexpr (N < sizeof...(Args))
  {
    auto& child = std::get<N>(children);

    if constexpr (std::is_base_of_v<SyntaxTree::BaseHandle,
      std::remove_reference_t<decltype(child)>>)
    {
      print_edge(o, level, parent, N, child);
    }
    else
    {
      std::visit([&o, &level, &parent](auto&& child)
      {
        if constexpr (std::is_base_of_v<SyntaxTree::BaseHandle,
          std::remove_reference_t<decltype(child)>>)
        {
          print_edge(o, level, parent, N, child);
        }
      }, child);
    }
    print_edges<N+1>(o, level, parent, children);
  }
  else if constexpr (N > 0)
  {
    o << "\n";
  }
}

std::ostream& operator<<(std::ostream& o, const SyntaxTree& tree)
{
  int level = 0;

  o << "digraph\n{\n  node[shape=record];\n\n";
  walk([&o, &level](auto&& handle)
  {
    o << std::string((level+1)*2, ' ');
    o << "n" << handle.index() << "[label=\"{" << get_name(handle);

    if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::PrimaryExpression>>)
    {
      o << "|" << handle->token;
    }
    else if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::MemberAccessExpression>>)
    {
      o << "|" << handle->type;
      o << "|" << handle->identifier.token;
    }
    else if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::UnaryExpression>>
      || std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::PostfixExpression>>
      || std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::BinaryExpression>>
      || std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::AssignmentExpression>>)
    {
      o << "|" << handle->type;
    }
    else if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::ArithmeticType>>)
    {
      if (handle->signed_token)
         {
        o << "|" << handle->signed_token;
      }
      if (handle->type_token)
      {
        o << "|" << handle->type_token;
      }
    }
    else if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::Enumerator>>)
    {
      o << "|" << handle->identifier.token;
    }
    else if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::StructUnion>>
      || std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::Enum>>)
    {
      o << "|" << handle->type;
      o << "|" << handle->identifier.token;
    }
    else if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::DeclarationSpecifiers>>)
    {
      if (handle->storage_class.token)
      {
        o << "|" << handle->storage_class.token;
      }
      if (handle->qualifiers.const_token)
      {
        o << "|" << handle->qualifiers.const_token;
      }
      if (handle->qualifiers.volatile_token)
      {
        o << "|" << handle->qualifiers.volatile_token;
      }
    }
    else if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::Pointer>>)
    {
      if (handle->qualifiers.const_token)
      {
        o << "|" << handle->qualifiers.const_token;
      }
      if (handle->qualifiers.volatile_token)
      {
        o << "|" << handle->qualifiers.volatile_token;
      }
    }
    else if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::DirectDeclarator>>)
    {
      if (std::holds_alternative<
        SyntaxTree::Identifier>(handle->variant))
      {
        o << "|" <<
          std::get<SyntaxTree::Identifier>(handle->variant).token;
      }
    }
    else if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::DeclarationSpecifiers>>)
    {
      if (handle->storage_class.token)
      {
        o << "|" << handle->storage_class.token;
      }
      if (handle->qualifiers.const_token)
      {
        o << "|" << handle->qualifiers.const_token;
      }
      if (handle->qualifiers.volatile_token)
      {
        o << "|" << handle->qualifiers.volatile_token;
      }
    }
    else if constexpr (std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::GotoTarget>>
      || std::is_same_v<
      std::remove_reference_t<decltype(handle)>,
      SyntaxTree::Handle<SyntaxTree::GotoStatement>>)
    {
      o << "|" << handle->identifier.token;
    }

    auto children = get_children(handle);

    print_anchors<0>(o, children);
    o << "}\"];\n";

    print_edges<0>(o, level, handle, children);
    ++level;
  },
  [&level](auto&&)
  {
    --level;
  },
  const_cast<SyntaxTree&>(tree).translation_unit);
  o << "}\n";

  return o;
}

