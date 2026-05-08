// Inverse Gaussian distribution (actuar R-level: mean + shape;
// dispersion = 1/shape).
//
//   f(x)   = (shape/(2*pi*x^3))^(1/2) * exp(-shape*(x-mean)^2 / (2*x*mean^2))
//
// d/p/q delegate to boost::math::inverse_gaussian_distribution(mean, shape).
// r matches actuar's rinvgauss: one rnorm + one runif call per sample.

#pragma once

#include "../special/constants.hpp"
#include <boost/math/distributions/inverse_gaussian.hpp>
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
  using namespace boost::math;
  const inverse_gaussian_distribution<> d(mean, shape);
  const double pdf_val = pdf(d, x);
  return log ? std::log(pdf_val) : pdf_val;
}

inline double pinvgauss(double q, double mean, double shape, bool lower_tail,
                        bool log_p) {
  if (!(mean > 0.0 && shape > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  using namespace boost::math;
  const inverse_gaussian_distribution<> d(mean, shape);
  double prob;
  if (q <= 0.0) {
    prob = lower_tail ? 0.0 : 1.0;
  } else {
    prob = lower_tail ? cdf(d, q) : cdf(complement(d, q));
  }
  return log_p ? std::log(prob) : prob;
}

inline double qinvgauss(double p, double mean, double shape, bool lower_tail,
                        bool log_p) {
  using namespace boost::math;
  if (!(mean > 0.0 && shape > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  const inverse_gaussian_distribution<> d(mean, shape);
  return lower_tail ? quantile(d, p) : quantile(complement(d, p));
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
