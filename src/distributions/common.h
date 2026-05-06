// Shared helpers for distribution exports.
#pragma once

#include <RcppArmadillo.h>
#include <Rmath.h>
#include <algorithm>
#include <limits>

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

inline double sign1(double x) { return (x > 0) - (x < 0); }
inline double heaviside(double x) {
  return x > 0 ? 1.0 : (x < 0 ? 0.0 : 0.5);
}

inline double ged_lambda(double nu) {
  return std::sqrt(std::pow(2.0, -2.0 / nu) * std::tgamma(1.0 / nu) /
                   std::tgamma(3.0 / nu));
}

inline double ged_g(double nu, double lambda) {
  return nu / (lambda * std::pow(2.0, 1.0 + 1.0 / nu) * std::tgamma(1.0 / nu));
}
