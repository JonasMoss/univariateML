// Inverse-Weibull fitting brackets the Gumbel profile on centered raw
// log ratios, with a checked restoration of the public scale.

#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"

#include "../../core/span.hpp"
#include "../../core/transformed_location.hpp"
#include "../gumbel/mle.hpp"
#include "../weibull/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class InvWeibullStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct InvWeibullResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  InvWeibullStatus status = InvWeibullStatus::Ok;
  bool certified=true;
};

inline FitOutcome outcome(const InvWeibullResult& r) {
  switch (r.status) {
    case InvWeibullStatus::Ok: return FitOutcome::estimate(r.certified);
    case InvWeibullStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case InvWeibullStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case InvWeibullStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
  }
  return {};
}

inline InvWeibullResult invweibull_mle(span x,double tol,int maxit,double shape,double scale) {
  const auto r=transformed_detail::location_fit(x,shape,scale,1,true,1,tol,maxit);
  return {r.shape,r.scale,r.ll,0,r.status==0?InvWeibullStatus::Ok:
    r.status==1?InvWeibullStatus::InvalidInput:r.status==2?InvWeibullStatus::Unbounded:InvWeibullStatus::IterationFailure,r.certified};
}
inline InvWeibullResult invweibull_mle(span x,double tol,int maxit) {
  return invweibull_mle(x,tol,maxit,NAN,NAN);
}
} // namespace cppunivariate
