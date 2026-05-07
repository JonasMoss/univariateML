// Shared helpers for distribution exports.
#pragma once

#include <Rcpp.h>
#include <algorithm>
#include <cstddef>
#include <limits>

#include "../ml/special/skewed_densities.hpp"
#include "../ml/special/special.hpp"

// Variadic recycle_length; returns the maximum of provided lengths.
template <typename... Lens>
inline std::size_t recycle_length(Lens... lens) {
  static_assert(sizeof...(lens) > 0, "recycle_length needs at least one arg");
  return std::max({static_cast<std::size_t>(lens)...});
}
