// Laplace MLE: median for location, mean absolute deviation for scale.

#pragma once
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../../core/outcome.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class LaplaceStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct LaplaceResult {
  double mu;
  double sigma;
  double logLik;
  LaplaceStatus status = LaplaceStatus::Ok;
  bool unique = true;
};

inline LaplaceResult laplace_mle(span x) {
  const std::size_t n = x.size();
  if (n == 0) {
    constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
    return {kNaN, kNaN, kNaN, LaplaceStatus::InvalidInput};
  }
  for (double v : x) if (!std::isfinite(v)) return {NAN,NAN,NAN,LaplaceStatus::InvalidInput};
  std::vector<double> sorted(x.begin(), x.end());
  std::sort(sorted.begin(), sorted.end());
  const double mu = sorted[n / 2];
  double abs_sum = 0.0;
  for (auto v : x) abs_sum += std::abs(v - mu);
  const double sigma = abs_sum / static_cast<double>(n);
  const double logLik =
      -static_cast<double>(n) * (1.0 + (std::log(2.0) + std::log(sigma)));
  return {mu, sigma, logLik, sigma==0 ? LaplaceStatus::Unbounded : std::isfinite(mu) && std::isfinite(sigma) && std::isfinite(logLik) ? LaplaceStatus::Ok : LaplaceStatus::IterationFailure,
          sorted[(n - 1) / 2] == sorted[n / 2]};
}


inline LaplaceResult laplace_mle(span x,double mu,double sigma) {
  if(std::isnan(mu)&&std::isnan(sigma))return laplace_mle(x);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.empty())return {nan,nan,nan,LaplaceStatus::InvalidInput};
  if ((!std::isnan(mu) && !std::isfinite(mu)) ||
      (!std::isnan(sigma) && !(sigma > 0 && std::isfinite(sigma))))
    return {nan,nan,nan,LaplaceStatus::InvalidInput};
  for (double v : x) if (!std::isfinite(v)) return {NAN,NAN,NAN,LaplaceStatus::InvalidInput};
  std::vector<double> y(x.begin(),x.end());std::sort(y.begin(),y.end());
  const double m=std::isnan(mu)?y[y.size()/2]:mu;
  CPPUNIV_EXTENDED_REAL a=0;for(double v:x)a+=std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m);
  const double s=std::isnan(sigma)?static_cast<double>(a/x.size()):sigma;
  const double ll=-static_cast<double>(x.size())*(std::log(2.0)+std::log(s))-static_cast<double>(a/s);
  return {m,s,ll,s==0 ? LaplaceStatus::Unbounded : std::isfinite(m) && std::isfinite(s) && std::isfinite(ll) ? LaplaceStatus::Ok : LaplaceStatus::IterationFailure,
          !std::isnan(mu) || y[(y.size()-1)/2]==y[y.size()/2]};
}

// Every point of the median interval maximizes the likelihood; `unique` says
// whether that interval is a single point.
inline FitOutcome outcome(const LaplaceResult& r) {
  switch (r.status) {
    case LaplaceStatus::Ok: return FitOutcome::estimate(true, r.unique);
    case LaplaceStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case LaplaceStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case LaplaceStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}

}  // namespace cppunivariate
