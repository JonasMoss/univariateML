// Normal MLE: closed form.
//
// The mean is formed from deviations from the first observation, which are
// exact for clustered data (Sterbenz), so the low bits of nearly tied or
// offset data survive; the sum of squares is scaled by the largest deviation
// so that data near 1e300 or 1e-300 neither overflow nor underflow.
//
// Standalone C++ implementation, free of R / Armadillo dependencies.
//
#pragma once
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class NormStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct NormResult {
  double mean;
  double sd;
  double logLik;
  NormStatus status = NormStatus::Ok;
};

namespace norm_detail {
struct Moments {
  double mean;
  double sd;
};

inline Moments moments(span x) {
  const std::size_t n = x.size();
  const double dn = static_cast<double>(n);
  double c = x[0];
  CPPUNIV_EXTENDED_REAL sum_d = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (auto v : x) sum_d += static_cast<CPPUNIV_EXTENDED_REAL>(v) - c;
  if (!std::isfinite(sum_d)) {
    // Deviations overflow (data of both signs near the double range).
    c = 0.0;
    sum_d = 0.0;
    for (auto v : x) sum_d += v / dn;
    sum_d *= dn;
  }
  const double mean_hat = static_cast<double>(c + sum_d / dn);
  // Profile scale at the representable mean actually returned. For a
  // cloud narrower than one ulp this matters to the reported likelihood.
  const CPPUNIV_EXTENDED_REAL mean_d = static_cast<CPPUNIV_EXTENDED_REAL>(mean_hat) - c;
  double big = 0.0;
  for (auto v : x) big = std::max(big, static_cast<double>(std::abs((static_cast<CPPUNIV_EXTENDED_REAL>(v) - c) - mean_d)));
  double ss = 0.0;
  if (big > 0.0 && std::isfinite(big)) {
    for (auto v : x) {
      const double z = ((static_cast<CPPUNIV_EXTENDED_REAL>(v) - c) - mean_d) / big;
      ss += z * z;
    }
  }
  return {mean_hat, big > 0.0 ? big * std::sqrt(ss / dn) : 0.0};
}
}  // namespace norm_detail

inline NormResult norm_mle(span x) {
  const std::size_t n = x.size();
  if (n == 0) {
    constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
    return {kNaN, kNaN, kNaN, NormStatus::InvalidInput};
  }
  for (double v : x) if (!std::isfinite(v)) return {NAN,NAN,NAN,NormStatus::InvalidInput};
  const auto m = norm_detail::moments(x);
  const double dn = static_cast<double>(n);
  const double logLik = -0.5 * dn * (std::log(2.0 * cppunivariate::pi) + 1.0) - dn * std::log(m.sd);
  return {m.mean, m.sd, logLik, m.sd==0 ? NormStatus::Unbounded : std::isfinite(m.mean) && std::isfinite(m.sd) && std::isfinite(logLik) ? NormStatus::Ok : NormStatus::IterationFailure};
}


// Conditional normal likelihood: exact mean and residual RMS profiles.
inline NormResult norm_mle(span x, double mean, double sd) {
  if (std::isnan(mean) && std::isnan(sd)) return norm_mle(x);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.empty()) return {nan,nan,nan,NormStatus::InvalidInput};
  if ((!std::isnan(mean) && !std::isfinite(mean)) ||
      (!std::isnan(sd) && !(sd > 0 && std::isfinite(sd))))
    return {nan,nan,nan,NormStatus::InvalidInput};
  for (double v : x) if (!std::isfinite(v)) return {NAN,NAN,NAN,NormStatus::InvalidInput};
  const double m=std::isnan(mean)?norm_detail::moments(x).mean:mean;
  CPPUNIV_EXTENDED_REAL ss=0; for(double v:x) {const CPPUNIV_EXTENDED_REAL d=static_cast<CPPUNIV_EXTENDED_REAL>(v)-m;ss+=d*d;}
  const double s=std::isnan(sd)?static_cast<double>(std::sqrt(ss/x.size())):sd;
  const double ll=-static_cast<double>(x.size())*(std::log(s)+.5*std::log(2*cppunivariate::pi))-static_cast<double>(ss/(2*static_cast<CPPUNIV_EXTENDED_REAL>(s)*s));
  return {m,s,ll,s==0 ? NormStatus::Unbounded : std::isfinite(m) && std::isfinite(s) && std::isfinite(ll) ? NormStatus::Ok : NormStatus::IterationFailure};
}

}  // namespace cppunivariate
