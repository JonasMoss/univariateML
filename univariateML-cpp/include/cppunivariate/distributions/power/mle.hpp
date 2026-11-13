// Power distribution MLE: closed form using log(x) statistics.
//
// alpha = max(x), beta = 1 / (log(alpha) - mean(log x)).
// Returns NaN params if min(x) <= 0; the caller turns that into an error.

#pragma once
#include "../../core/outcome.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class PowerStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct PowerResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
  PowerStatus status = PowerStatus::Ok;
};

inline FitOutcome outcome(const PowerResult& r) {
  switch (r.status) {
    case PowerStatus::Ok: return FitOutcome::estimate(true);
    case PowerStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case PowerStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case PowerStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}

// epsilon is retained for source compatibility and no longer moves endpoints.
inline PowerResult power_mle(span x, double epsilon, double alpha, double beta) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const auto invalid = PowerResult{nan, nan, nan, 0, PowerStatus::InvalidInput};
  if (x.size() == 0 || !(epsilon >= 0) || !std::isfinite(epsilon)) return invalid;
  double xmax = 0;
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v)) return invalid;
    xmax = std::max(xmax, v);
  }
  if (std::isnan(alpha)) alpha = xmax;
  if (!(alpha >= xmax) || !std::isfinite(alpha) ||
      (!std::isnan(beta) && (!(beta > 0) || !std::isfinite(beta)))) return invalid;
  CPPUNIV_EXTENDED_REAL gap = 0, logs = 0;
  for (double v : x) {
    gap += std::log1p((static_cast<CPPUNIV_EXTENDED_REAL>(alpha) - v) / v);
    logs += std::log(v);
  }
  const double n = static_cast<double>(x.size());
  if (std::isnan(beta)) {
    if (!(gap > 0)) return {alpha, nan, nan, 0, PowerStatus::Unbounded};
    beta = static_cast<double>(n / gap);
  }
  const double ll = static_cast<double>(n * std::log(beta) - beta * gap - logs);
  return {alpha, beta, ll, 0, std::isfinite(beta) && std::isfinite(ll)
      ? PowerStatus::Ok : PowerStatus::IterationFailure};
}

inline PowerResult power_mle(span x, double epsilon = 0) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  return power_mle(x, epsilon, nan, nan);
}

}  // namespace cppunivariate
