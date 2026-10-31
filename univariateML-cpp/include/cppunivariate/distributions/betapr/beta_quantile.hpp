#pragma once
#include "../../special/beta_tails.hpp"
#include "../../special/tails.hpp"
#include <cmath>
namespace cppunivariate { namespace betapr_detail {
// Boost's no-throw inverse can return a finite but inaccurate extreme-tail
// candidate. Check it in log probability before accepting it; otherwise solve
// with the beta log-tail continuation, keeping both odds coordinates accurate.
inline double beta_logit_quantile(double lp, double a, double b, bool lower) {
  if (lp == special::kNegInf) return lower ? special::kNegInf : special::kPosInf;
  const auto value = [=](double z) {
    return missing_detail::beta_log(-special::softplus(-z),
        -special::softplus(z), a, b, lower);
  };
  // Deep-tail Boost inverses can throw from an internal default-policy root
  // solver even through the no-throw wrapper. Use continuation there directly.
  const bool ordinary = lp > -36.0 && a >= 0.1 && b >= 0.1 && a <= 1000.0 && b <= 1000.0;
  const double candidate = ordinary ? missing_detail::beta_logit_q(lp, a, b, lower)
                                    : special::kNaN;
  if (std::isfinite(candidate) &&
      std::abs(value(candidate)-lp) <= 1e-13*std::fmax(1.0,std::abs(lp)))
    return candidate;
  // Beyond these log odds, multiplying by any finite positive scale still
  // rounds the final beta-prime quantile to zero or infinity.
  double lo = -1500.0, hi = 1500.0;
  const double left = value(lo), right = value(hi);
  if (std::isnan(left) || std::isnan(right)) return special::kNaN;
  if (lower ? left >= lp : left <= lp) return special::kNegInf;
  if (lower ? right <= lp : right >= lp) return special::kPosInf;
  for (int i=0; i<120; ++i) {
    const double mid = lo*0.5+hi*0.5;
    if (mid == lo || mid == hi) break;
    const double at_mid = value(mid);
    if (std::isnan(at_mid)) return special::kNaN;
    if (lower ? at_mid < lp : at_mid > lp) lo = mid;
    else hi = mid;
  }
  return lo*0.5+hi*0.5;
}
} }
