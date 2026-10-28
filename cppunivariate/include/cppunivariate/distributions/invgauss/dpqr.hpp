// Inverse Gaussian distribution (actuar R-level: mean + shape;
// dispersion = 1/shape).
//
//   f(x)   = (shape/(2*pi*x^3))^(1/2) * exp(-shape*(x-mean)^2 / (2*x*mean^2))
//
// d/p/q delegate to the inverse Gaussian primitives in the special seam.
// r matches actuar's rinvgauss: one rnorm + one runif call per sample.

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

namespace detail {
constexpr double kInvGaussYphiLarge = 5e5;
} // namespace detail

inline double dinvgauss(double x, double mean, double shape, bool log) {
  if (!(mean > 0.0 && shape > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x <= 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double pdf_val = cppunivariate::special::inverse_gaussian_pdf(x, mean, shape);
  return log ? std::log(pdf_val) : pdf_val;
}

inline double pinvgauss(double q, double mean, double shape, bool lower_tail,
                        bool log_p) {
  if (!(mean > 0.0 && shape > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = lower_tail ? 0.0 : 1.0;
  } else {
    prob = cppunivariate::special::inverse_gaussian_cdf(q, mean, shape, lower_tail);
  }
  return log_p ? std::log(prob) : prob;
}

inline double qinvgauss(double p, double mean, double shape, bool lower_tail,
                        bool log_p) {
  if (!(mean > 0.0 && shape > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  return cppunivariate::special::inverse_gaussian_quantile(p, mean, shape, lower_tail);
}

template <typename Rng>
inline double rinvgauss(Rng &rng, double mean, double shape) {
  // Bit-for-bit port of actuar's rinvgauss: phi = 1/shape (dispersion form).
  const double phi = 1.0 / shape;
  const double z = rng.rnorm();
  const double yphi = z * z * phi * mean;
  double x;
  if (yphi > detail::kInvGaussYphiLarge) {
    x = 1.0 / yphi;
  } else {
    x = 1.0 + yphi / 2.0 * (1.0 - std::sqrt(1.0 + 4.0 / yphi));
  }
  const double u = rng.runif(0.0, 1.0);
  return mean * ((u <= 1.0 / (1.0 + x)) ? x : 1.0 / x);
}

} // namespace distributions
} // namespace cppunivariate
