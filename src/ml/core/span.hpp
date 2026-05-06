// Minimal read-only view over contiguous double data. C++17-compatible
// substitute for std::span<const double> (which is C++20). Replace with
// std::span<const double> when we bump the standard.
//
// Lifetime: span does not own its storage. The caller must keep the
// underlying buffer alive for the duration of any call that receives the
// span.

#pragma once

#include <cstddef>

namespace mlcore {

struct span {
  const double *p;
  std::size_t n;

  constexpr const double *data() const noexcept { return p; }
  constexpr std::size_t size() const noexcept { return n; }
  constexpr bool empty() const noexcept { return n == 0; }
  constexpr const double &operator[](std::size_t i) const noexcept {
    return p[i];
  }
  constexpr const double *begin() const noexcept { return p; }
  constexpr const double *end() const noexcept { return p + n; }
};

}  // namespace mlcore
