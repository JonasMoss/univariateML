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
#include "../../core/outcome.hpp"

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

// Residuals are divided by their largest magnitude before squaring.
// Halving is needed only when subtraction itself exceeds the working range.
struct Residuals {
  double big, square, factor;
  bool zero;
  double rms(std::size_t n) const {
    return (big * std::sqrt(square / static_cast<double>(n))) / factor;
  }
  double standardized_square(double sd) const {
    const CPPUNIV_EXTENDED_REAL ratio =
        (static_cast<CPPUNIV_EXTENDED_REAL>(big) / sd) / factor;
    return static_cast<double>(square * ratio * ratio);
  }
};
inline Residuals residuals(span x, double center) {
  double big = 0, factor = 1;
  bool zero = true;
  for (double v : x) {
    zero = zero && v == center;
    const double d = static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(v) - center);
    big = std::max(big, std::abs(d));
  }
  if (!std::isfinite(big)) {
    factor = 0.5; big = 0;
    for (double v : x) big = std::max(big, std::abs(v * factor - center * factor));
  }
  double square = 0;
  if (big > 0) for (double v : x) {
    const double z = factor == 1 ?
        static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(v) - center) / big) :
        (v * factor - center * factor) / big;
    square += z * z;
  }
  return {big, square, factor, zero};
}

inline Moments moments(span x) {
  const std::size_t n = x.size();
  const double dn = static_cast<double>(n);
  double c = x[0];
  CPPUNIV_EXTENDED_REAL sum_d = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (auto v : x) sum_d += static_cast<CPPUNIV_EXTENDED_REAL>(v) - c;
  double mean_hat;
  if (!std::isfinite(sum_d)) {
    // Deviations overflow (data of both signs near the double range).
    c = 0.0;
    sum_d = 0.0;
    for (auto v : x) sum_d += v / dn;
    mean_hat = static_cast<double>(sum_d);
  } else mean_hat = static_cast<double>(c + sum_d / dn);
  // Profile at the representable mean actually returned, including near ties.
  return {mean_hat, residuals(x, mean_hat).rms(n)};
}

// v - mean_hat split exactly into a double and its rounding error (TwoSum).
inline double residual_error(double v, double mean_hat, double d) {
  const double back = d - v;
  return (v - (d - back)) + (-mean_hat - back);
}

// Rounding error of a returned mean: the exact sample mean minus mean_hat.
// Residuals about mean_hat are split exactly and summed with compensation,
// so the result stays accurate far below the spacing of mean_hat. Zero when
// a residual overflows; the correction is then negligible against the spread.
inline double mean_rounding(span x, double mean_hat) {
  double hi = 0, lo = 0;
  for (double v : x) {
    const double d = v - mean_hat;
    if (!std::isfinite(d)) return 0;
    const double t = hi + d, back = t - hi;
    lo += residual_error(v, mean_hat, d) + ((hi - (t - back)) + (d - back));
    hi = t;
  }
  return (hi + lo) / static_cast<double>(x.size());
}

// Root mean square of the residuals about the exact mean mean_hat + delta.
inline double centered_rms(span x, double mean_hat, double delta) {
  const auto residual = [&](double v) {
    const double d = v - mean_hat;
    return d + (residual_error(v, mean_hat, d) - delta);
  };
  double big = 0;
  for (double v : x) big = std::max(big, std::abs(residual(v)));
  if (!(big > 0)) return 0;
  double square = 0;
  for (double v : x) { const double z = residual(v) / big; square += z * z; }
  return big * std::sqrt(square / static_cast<double>(x.size()));
}

// Residuals about the rounded mean overstate the MLE sd: sd_r^2 = sd^2 +
// delta^2, delta the rounding error of the mean. For |delta| <= 2^-27 sd_r
// the relative effect is below 2^-55 and sd_r stands; beyond it (near ties,
// e.g. (1, 1, 1 + 2^-52): sd_r = 2^-52/sqrt(3), sd = 2^-52 sqrt(2)/3) the
// residuals are recomputed about the exact mean.
inline double exact_center_rms(span x, double mean_hat, double rms_rounded) {
  const double delta = mean_rounding(x, mean_hat);
  if (!(std::abs(delta) > 0x1p-27 * rms_rounded)) return rms_rounded;
  return centered_rms(x, mean_hat, delta);
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
  // The MLE sd is the residual RMS about the exact mean, not the rounded one.
  const double sd = norm_detail::exact_center_rms(x, m.mean, m.sd);
  const double dn = static_cast<double>(n);
  const double logLik = -0.5 * dn * (std::log(2.0 * cppunivariate::pi) + 1.0) - dn * std::log(sd);
  return {m.mean, sd, logLik, std::all_of(x.begin(),x.end(),[&](double v){return v==m.mean;}) ? NormStatus::Unbounded : std::isfinite(m.mean) && sd>=std::numeric_limits<double>::min() && std::isfinite(sd) && std::isfinite(logLik) ? NormStatus::Ok : NormStatus::IterationFailure};
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
  const auto residual = norm_detail::residuals(x,m);
  if (std::isnan(sd) && residual.zero) return {m,0,INFINITY,NormStatus::Unbounded};
  const double s=std::isnan(sd)?residual.rms(x.size()):sd;
  if (!(s>0 && std::isfinite(s)) || (std::isnan(sd) && s<std::numeric_limits<double>::min())) return {m,s,nan,NormStatus::IterationFailure};
  // With the mean free, the maximized value uses residuals about the exact
  // mean (see exact_center_rms); the rounded mean would lower it near ties.
  const double rms=residual.rms(x.size());
  const double exact=std::isnan(mean)?norm_detail::exact_center_rms(x,m,rms):rms;
  const double penalty=exact==rms?residual.standardized_square(s):
      static_cast<double>(x.size())*(exact/s)*(exact/s);
  const double ll=-static_cast<double>(x.size())*(std::log(s)+.5*std::log(2*cppunivariate::pi))-
      .5*penalty;
  return {m,s,ll,std::isfinite(m) && std::isfinite(ll) ? NormStatus::Ok : NormStatus::IterationFailure};
}

inline FitOutcome outcome(const NormResult& r) {
  switch (r.status) {
    case NormStatus::Ok: return FitOutcome::estimate();
    case NormStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case NormStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case NormStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}

}  // namespace cppunivariate
