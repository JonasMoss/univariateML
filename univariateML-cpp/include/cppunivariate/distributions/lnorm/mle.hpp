// Lognormal MLE: y = log(x), then Normal MLE on y. Adjusted log-likelihood
// includes the change-of-variable Jacobian -sum(log(x)).

#pragma once
#include "../../core/outcome.hpp"
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../../core/transformed_data.hpp"
#include "../norm/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class LognormalStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct LognormalResult {
  double meanlog;
  double sdlog;
  double logLik;
  LognormalStatus status = LognormalStatus::Ok;
  bool certified=true;
};

inline LognormalResult lognormal_mle(span x,double meanlog,double sdlog) {
  const auto r=transformed_detail::normal(transformed_detail::logs(x),meanlog,sdlog);
  return {r.mean,r.sd,r.ll,r.status==0?LognormalStatus::Ok:
    r.status==1?LognormalStatus::InvalidInput:r.status==2?LognormalStatus::Unbounded:LognormalStatus::IterationFailure,r.certified};
}
inline LognormalResult lognormal_mle(span x) { return lognormal_mle(x,NAN,NAN); }
} // namespace cppunivariate

namespace cppunivariate {
inline FitOutcome outcome(const LognormalResult& r) {
  switch (r.status) {
    case LognormalStatus::Ok: return FitOutcome::estimate(r.certified);
    case LognormalStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case LognormalStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case LognormalStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
} // namespace cppunivariate
