// Shape score of the smooth Prentice log density, including Q=0.
#pragma once
#include "held.hpp"

namespace cppunivariate {
namespace gengamma_fit_detail {

struct ShapeScore { double value; bool valid; };

// Derivative of the Stirling remainder R(Q^-2). For |Q| <= .25 the
// first omitted term of the alternating derivative expansion is bounded
// in magnitude by |Q|^25/6 in exact arithmetic.
inline double shape_remainder_derivative(double q) {
  const double q2 = q*q;
  if (std::abs(q) <= 0.25) {
    const double q4 = q2*q2;
    return q * (1.0/6 + q4 * (-1.0/60 + q4 * (1.0/126 + q4 *
        (-1.0/120 + q4 * (1.0/66 - q4*691.0/16380)))));
  }
  const double a = (1/q)/q;
  return (2/q) * ((trgamma_detail::log_minus_digamma(a)/q)/q - 0.5);
}

// Derivative of (exp(Q*w)-1-Q*w)/Q^2. The polynomial is the Taylor
// series through z^9, with z=Q*w; the next term is 11*z^10/13!.
inline double shape_penalty_derivative(double w, double q) {
  const double z = q*w;
  if (std::abs(z) <= 0.1) {
    const double poly = 1.0/6 + z*(1.0/12 + z*(1.0/40 + z*(1.0/180 +
        z*(1.0/1008 + z*(1.0/6720 + z*(1.0/51840 + z*(1.0/453600 +
        z*(1.0/4435200 + z/47900160))))))));
    return ((w*w)*w)*poly;
  }
  const double r = std::expm1(z);
  return ((z*r - 2*(r-z))/q/q)/q;
}

inline ShapeScore shape_score(span y, double mu, double sigma, double q) {
  if (y.empty() || !std::isfinite(mu) || !(sigma > 0) || !std::isfinite(sigma) || !std::isfinite(q))
    return {std::numeric_limits<double>::quiet_NaN(), false};
  CPPUNIV_EXTENDED_REAL sum = 0;
  for (double v : y) {
    if (!std::isfinite(v)) return {std::numeric_limits<double>::quiet_NaN(), false};
    const double term = shape_penalty_derivative((v-mu)/sigma, q);
    if (!std::isfinite(term)) return {std::numeric_limits<double>::quiet_NaN(), false};
    sum += term;
  }
  const double value = -shape_remainder_derivative(q) - static_cast<double>(sum/y.size());
  return {value, std::isfinite(value)};
}

}  // namespace gengamma_fit_detail
}  // namespace cppunivariate
