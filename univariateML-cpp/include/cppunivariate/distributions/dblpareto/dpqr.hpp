#pragma once
#include "../../special/tails.hpp"

namespace cppunivariate::distributions {

inline bool dblpareto_parameters(double scale, double left_rate, double right_rate) {
  return scale > 0 && left_rate > 0 && right_rate > 0 &&
    std::isfinite(scale) && std::isfinite(left_rate) && std::isfinite(right_rate);
}

// Log of small/(small+large), then log of large/(small+large).
inline special::LogTails dblpareto_masses(double left_rate, double right_rate) {
  const double small = std::min(left_rate, right_rate), large = std::max(left_rate, right_rate);
  const double correction = -std::log1p(small / large);
  const double log_small = special::log_positive_ratio(small, large) + correction;
  return left_rate >= right_rate ? special::LogTails{log_small, correction, true}
                                : special::LogTails{correction, log_small, true};
}

inline double ddblpareto(double x, double scale, double left_rate, double right_rate, bool log) {
  if (!dblpareto_parameters(scale, left_rate, right_rate) || std::isnan(x)) return special::kNaN;
  if (!(x > 0) || !std::isfinite(x)) return log ? special::kNegInf : 0;
  const double small = std::min(left_rate, right_rate), large = std::max(left_rate, right_rate);
  const double gap = special::log_positive_ratio(x, scale);
  const double lp = special::log_positive_ratio(small, x) - std::log1p(small / large)
      + (x <= scale ? left_rate : -right_rate) * gap;
  return log ? lp : std::exp(lp);
}

inline double pdblpareto(double q, double scale, double left_rate, double right_rate,
                        bool lower_tail, bool log_p) {
  if (!dblpareto_parameters(scale, left_rate, right_rate) || std::isnan(q)) return special::kNaN;
  if (!(q > 0) || !std::isfinite(q))
    return special::tail_probability(special::tails_from_log(special::kNegInf, q <= 0),
                                     lower_tail, log_p);
  const auto mass = dblpareto_masses(left_rate, right_rate);
  const bool left = q <= scale;
  const double gap = std::abs(special::log_positive_ratio(q, scale));
  const double rate = left ? left_rate : right_rate;
  const double log_mass = left ? mass.lower : mass.upper;
  const double log_hazard = std::log(rate) + std::log(gap);
  const double tail = log_mass - rate * gap;
  if (tail < -0.6931471805599453)
    return special::tail_probability(special::tails_from_log(tail, left), lower_tail, log_p);
  const double base = left ? mass.upper : mass.lower;
  const double added =
      log_mass + (log_hazard < -36 ? log_hazard : special::log1mexp(-rate * gap));
  const double other = std::max(base, added) + std::log1p(std::exp(-std::abs(base - added)));
  const special::LogTails tails = left ? special::LogTails{tail, other, true}
                                      : special::LogTails{other, tail, true};
  return special::tail_probability(tails, lower_tail, log_p);
}

inline double qdblpareto(double p, double scale, double left_rate, double right_rate,
                        bool lower_tail, bool log_p) {
  if (!dblpareto_parameters(scale, left_rate, right_rate)) return special::kNaN;
  const auto t = special::quantile_log_tails(p, lower_tail, log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower == special::kNegInf) return 0;
  if (t.upper == special::kNegInf) return special::kPosInf;
  const auto mass = dblpareto_masses(left_rate, right_rate);
  const bool left_smaller = left_rate >= right_rate;
  const bool left = left_smaller ? t.lower <= mass.lower : t.upper >= mass.upper;
  double offset = left ? (t.lower - mass.lower) / left_rate
                       : (mass.upper - t.upper) / right_rate;
  if (left_rate == right_rate) {
    const double centered = special::probability_minus_half(p, lower_tail, log_p);
    if (std::abs(centered) < 1e-8)
      offset = std::copysign(-std::log1p(-2 * std::abs(centered)), centered) / left_rate;
  }
  if (std::abs(offset) < 1) return std::fma(scale, std::expm1(offset), scale);
  return std::exp(std::log(scale) + offset);
}

template <class Rng>
inline double rdblpareto(Rng& rng, double scale, double left_rate, double right_rate) {
  return qdblpareto(rng.runif(0, 1), scale, left_rate, right_rate, true, false);
}

} // namespace cppunivariate::distributions
