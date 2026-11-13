// Rayleigh MLE: closed form. sigma = sqrt(mean(x^2) / 2), with the sum of
// squares scaled by max(x) so that data near 1e300 or 1e-300 neither
// overflow nor underflow.

#pragma once
#include "../../core/outcome.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class RayleighStatus { Ok, InvalidData, ZeroLikelihood };

struct RayleighResult {
  double sigma;
  double logLik;
  RayleighStatus status = RayleighStatus::Ok;
};

inline RayleighResult rayleigh_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, RayleighStatus::InvalidData};

  double big = 0.0;
  for (auto v : x) {
    if (v == 0.0) return {kNaN, kNaN, RayleighStatus::ZeroLikelihood};
    if (!(v > 0.0) || !std::isfinite(v)) return {kNaN, kNaN, RayleighStatus::InvalidData};
    big = std::max(big, v);
  }
  if (!(big > 0.0) || !std::isfinite(big)) return {kNaN, kNaN, RayleighStatus::InvalidData};
  double ss = 0.0;
  for (auto v : x) {
    const double z = v / big;
    ss += z * z;
  }
  const double dn = static_cast<double>(n);
  const double sigma = big * std::sqrt(ss / (2.0 * dn));
  double sum_log = 0.0;
  for (auto v : x) sum_log += std::log(v);
  // sum(x^2) / (2 sigma^2) = n at the MLE.
  const double logLik = sum_log - 2.0 * dn * std::log(sigma) - dn;
  return {sigma, logLik};
}

inline RayleighResult rayleigh_mle(span x,double sigma) {
  if(std::isnan(sigma))return rayleigh_mle(x);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(!(sigma>0 && std::isfinite(sigma)) || x.size()==0)return {nan,nan,RayleighStatus::InvalidData};
  CPPUNIV_EXTENDED_REAL ll=0;for(double v:x){if(v==0)return {nan,nan,RayleighStatus::ZeroLikelihood};if(!(v>0 && std::isfinite(v)))return {nan,nan,RayleighStatus::InvalidData};const CPPUNIV_EXTENDED_REAL z=static_cast<CPPUNIV_EXTENDED_REAL>(v)/sigma;ll+=std::log(v)-2*std::log(sigma)-z*z/2;}
  return {sigma,static_cast<double>(ll),RayleighStatus::Ok};
}

}  // namespace cppunivariate

namespace cppunivariate {
inline FitOutcome outcome(const RayleighResult& r) {
  switch (r.status) {
    case RayleighStatus::Ok: return FitOutcome::estimate(true);
    case RayleighStatus::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case RayleighStatus::ZeroLikelihood: return FitOutcome::nonexistence(OutcomeReason::Unattained);
  }
  return {};
}
} // namespace cppunivariate
