  //| //|
 //||//|| Martin Falch, 2018.
// ||/ || GNU GPLv3

#pragma once

#include <array>
#include <stdexcept>

template <typename T, unsigned int N>
class CircularBuffer
{
  public:
  struct Error : public std::runtime_error
  {
    using std::runtime_error::runtime_error;
  };

  CircularBuffer()
    :
    head(0U),
    tail(0U),
    full(false)
  { }

  void push(const T& element)
  {
    if (full)
    {
      throw Error("CircularBuffer is full!");
    }
    data.at(head) = element;
    head = advance(head);
    full = (head == tail);
  }

  T pop()
  {
    if (size() == 0)
    {
      throw Error("CircularBuffer is empty.");
    }
    T element = data.at(tail);
    tail = advance(tail);
    full = false;
    return element;
  }

  T peek(unsigned int position) const
  {
    if (position >= size())
    {
      throw Error("Tried to peek to deep into CircularBuffer.");
    }
    position = advance(tail, position);
    return data.at(position);
  }

  unsigned int size() const
  {
    return full ? N : (head-tail) % N;
  }

  void clear()
  {
    tail = head;
    full = false;
  }

  protected:
  static unsigned int advance(unsigned int position, unsigned int amount = 1)
  {
    return (position + amount) % N;
  }

  std::array<T, N> data;
  unsigned int head;
  unsigned int tail;
  bool full;
};

