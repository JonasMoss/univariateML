#pragma once
#include "../../special/tails.hpp"
#include <cmath>
namespace cppunivariate { namespace distributions {
namespace llogis_detail {
inline double log_odds(double p, bool lower, bool log_p,
                       const special::LogTails &tails) {
  const double centered = 2*special::probability_minus_half(p,lower,log_p);
  return std::abs(centered)<=0.5 ? 2*std::atanh(centered) : tails.lower-tails.upper;
}
}

inline double dllogis(double x, double shape, double scale, bool log) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const double rate = 1.0 / scale;
  const double log_rate = std::isfinite(rate) ? std::log(rate) : -std::log(scale);
  if (!(shape > 0.0 && std::isfinite(shape)) || std::isnan(x)) return special::kNaN;
  if (x < 0.0 || x == special::kPosInf) return log ? special::kNegInf : 0.0;
  if (x == 0.0) {
    if (shape < 1.0) return special::kPosInf;
    if (shape > 1.0) return log ? special::kNegInf : 0.0;
    return log ? log_rate : rate;
  }
  const double z = shape * special::log_positive_ratio(x, scale);
  const double ld = std::log(shape) - std::log(x) - special::softplus(z) - special::softplus(-z);
  return log ? ld : std::exp(ld);
}
inline double pllogis(double q, double shape, double scale, bool lower_tail, bool log_p) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  if (!(shape > 0.0 && std::isfinite(shape)) || std::isnan(q)) return special::kNaN;
  if (q <= 0.0) return special::tail_probability(special::tails_from_log(special::kNegInf, true), lower_tail, log_p);
  const double z = shape * special::log_positive_ratio(q, scale);
  return special::tail_probability({-special::softplus(-z), -special::softplus(z), true}, lower_tail, log_p);
}
inline double qllogis(double p, double shape, double scale, bool lower_tail, bool log_p) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const double rate = 1.0 / scale;
  const double log_rate = std::isfinite(rate) ? std::log(rate) : -std::log(scale);
  if (!(shape > 0.0 && std::isfinite(shape))) return special::kNaN;
  const auto t = special::quantile_log_tails(p, lower_tail, log_p);
  if (!t.valid) return special::kNaN;
  return std::exp(llogis_detail::log_odds(p,lower_tail,log_p,t) / shape - log_rate);
}
template <typename Rng>
inline double rllogis(Rng &rng, double shape, double scale) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  if (!(shape > 0.0 && std::isfinite(shape))) return special::kNaN;
  const double u = rng.runif(0.0, 1.0);
  return scale * std::pow(u / (1.0 - u), 1.0 / shape);
}


} }
