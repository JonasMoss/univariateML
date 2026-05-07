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

inline arma::vec scalar_vec(double value) {
  arma::vec v(1);
  v[0] = value;
  return v;
}

// Re-export the pure-libm helpers at file scope so existing call sites in
// the distributions/*.cpp files keep working unchanged.
using mlcore::dens::sign1;
using mlcore::dens::heaviside;
using mlcore::dens::ged_lambda;
using mlcore::dens::ged_g;
