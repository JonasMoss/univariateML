#pragma once
#include "../../special/tails.hpp"
#include <cmath>
namespace cppunivariate { namespace distributions {
namespace llogis_detail {
// Use the product when representable to avoid cancellation of large logs.
inline double log_positive_product(double a, double b) {
  const double product = a*b;
  return product >= std::numeric_limits<double>::min() && std::isfinite(product)
      ? std::log(product) : std::log(a)+std::log(b);
}

}

inline double dllogis(double x, double shape, double rate, bool log) {
  if (!(shape > 0.0 && std::isfinite(shape) && rate > 0.0 && std::isfinite(rate)) || std::isnan(x)) return special::kNaN;
  if (x < 0.0 || x == special::kPosInf) return log ? special::kNegInf : 0.0;
  if (x == 0.0) {
    if (shape < 1.0) return special::kPosInf;
    if (shape > 1.0) return log ? special::kNegInf : 0.0;
    return log ? std::log(rate) : rate;
  }
  const double z = shape * (llogis_detail::log_positive_product(x, rate));
  const double ld = std::log(shape) - std::log(x) - special::softplus(z) - special::softplus(-z);
  return log ? ld : std::exp(ld);
}
inline double pllogis(double q, double shape, double rate, bool lower_tail, bool log_p) {
  if (!(shape > 0.0 && std::isfinite(shape) && rate > 0.0 && std::isfinite(rate)) || std::isnan(q)) return special::kNaN;
  if (q <= 0.0) return special::tail_probability(special::tails_from_log(special::kNegInf, true), lower_tail, log_p);
  const double z = shape * (llogis_detail::log_positive_product(q, rate));
  return special::tail_probability({-special::softplus(-z), -special::softplus(z), true}, lower_tail, log_p);
}
inline double qllogis(double p, double shape, double rate, bool lower_tail, bool log_p) {
  if (!(shape > 0.0 && std::isfinite(shape) && rate > 0.0 && std::isfinite(rate))) return special::kNaN;
  const auto t = special::quantile_log_tails(p, lower_tail, log_p);
  if (!t.valid) return special::kNaN;
  return std::exp((t.lower - t.upper) / shape - std::log(rate));
}
template <typename Rng>
inline double rllogis(Rng &rng, double shape, double rate) {
  const double u = rng.runif(0.0, 1.0);
  const double scale = 1.0 / rate;
  return scale * std::pow(u / (1.0 - u), 1.0 / shape);
}


} }
