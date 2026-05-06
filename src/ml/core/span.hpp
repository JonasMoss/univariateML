// Minimal read-only view over contiguous data. C++17-compatible substitute
// for std::span<const T> (which is C++20). Replace with std::span<const T>
// when we bump the standard.
//
// Lifetime: span does not own its storage. The caller must keep the
// underlying buffer alive for the duration of any call that receives the
// span.

#pragma once

#include <cstddef>

namespace mlcore {

template <typename T>
struct basic_span {
  const T *p;
  std::size_t n;

  constexpr const T *data() const noexcept { return p; }
  constexpr std::size_t size() const noexcept { return n; }
  constexpr bool empty() const noexcept { return n == 0; }
  constexpr const T &operator[](std::size_t i) const noexcept { return p[i]; }
  constexpr const T *begin() const noexcept { return p; }
  constexpr const T *end() const noexcept { return p + n; }
};

using span = basic_span<double>;
using ispan = basic_span<int>;

}  // namespace mlcore
