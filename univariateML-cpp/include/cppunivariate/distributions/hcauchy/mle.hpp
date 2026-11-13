// Half-Cauchy MLE, with an optional translated threshold mu (C5). nu is
// always held at 1 in the delegated half-t engine, so the nu >= 1
// restriction that `ht_mle` applies for a free threshold is always
// satisfied trivially here: holding mu = min(x) (or a supplied mu) and
// delegating to `ht_mle_zero` on x - mu reproduces exactly the existing
// zero-threshold Ok/Unbounded/ScaleBoundary logic, now keyed on the
// multiplicity of the (free or held) endpoint rather than literal zeros.
// See notes/evaluations/2026-10-29-p0-halfthreshold.md ("hcauchy
// (translated)") and 2026-10-29-p4-report.md.
#pragma once
#include "../../core/outcome.hpp"
#include "../ht/mle.hpp"
#include <algorithm>
namespace cppunivariate {
enum class HcauchyStatus { Ok, InvalidData, Unbounded, ScaleBoundary, SearchFailure };
struct HcauchyResult { double mu, sigma, logLik; int iterations; HcauchyStatus status; };

inline FitOutcome outcome(const HcauchyResult& r) {
  switch (r.status) {
    case HcauchyStatus::Ok: return FitOutcome::estimate(true);
    case HcauchyStatus::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case HcauchyStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case HcauchyStatus::ScaleBoundary: return FitOutcome::nonexistence(OutcomeReason::Unattained);
    case HcauchyStatus::SearchFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
  }
  return {};
}
inline HcauchyResult hcauchy_mle(span x, double held_mu = std::numeric_limits<double>::quiet_NaN(),
                                  double held_sigma = std::numeric_limits<double>::quiet_NaN()) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (x.size() == 0) return {nan,nan,nan,0,HcauchyStatus::InvalidData};
  for (double v : x) if (!std::isfinite(v)) return {nan,nan,nan,0,HcauchyStatus::InvalidData};
  const bool mu_free = std::isnan(held_mu);
  double mu_hat;
  if (mu_free) {
    mu_hat = *std::min_element(x.begin(), x.end());
  } else {
    if (!std::isfinite(held_mu)) return {held_mu,nan,nan,0,HcauchyStatus::InvalidData};
    mu_hat = held_mu;
    for (double v : x) if (v < mu_hat) return {mu_hat,nan,nan,0,HcauchyStatus::InvalidData};
  }
  std::vector<double> y(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) y[i] = x[i]-mu_hat;
  const auto r = ht_mle_zero(span{y.data(),y.size()}, 1.0, held_sigma);
  HcauchyStatus s = HcauchyStatus::SearchFailure;
  if (r.status==HtStatus::Ok) s = HcauchyStatus::Ok;
  if (r.status==HtStatus::InvalidData) s = HcauchyStatus::InvalidData;
  if (r.status==HtStatus::Unbounded) s = HcauchyStatus::Unbounded;
  if (r.status==HtStatus::ScaleBoundary) s = HcauchyStatus::ScaleBoundary;
  return {mu_hat,r.sigma,r.logLik,r.iterations,s};
}
} // namespace cppunivariate
