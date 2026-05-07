// Shared helpers for distribution exports.
#pragma once

#include <RcppArmadillo.h>
#include <algorithm>
#include <limits>

#include "../ml/special/skewed_densities.hpp"
#include "../ml/special/special.hpp"

using arma::uword;

// Variadic recycle_length; returns the maximum of provided lengths.
template <typename... Lens> inline uword recycle_length(Lens... lens) {
  static_assert(sizeof...(lens) > 0, "recycle_length needs at least one arg");
  return std::max({static_cast<uword>(lens)...});
}
