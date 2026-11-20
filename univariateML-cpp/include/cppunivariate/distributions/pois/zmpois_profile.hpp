// Compatibility forwarding header; implementation lives in core.
#pragma once
#include "../../core/outcome.hpp"
#include "../../core/zero_modified_fit.hpp"
#include <cmath>
#include <limits>

namespace cppunivariate {
// ZmpoisResult, ZmgeomResult and ZmlogarithmicResult are aliases of this type.
// Keep their single overload in this shared family forwarding header.
inline FitOutcome outcome(const zero_detail::ModifiedResult& r) {
  if(r.stop_detail.accuracy_stop==AccuracyStop::Tolerance)return FitOutcome::failure(OutcomeReason::Tolerance);
  if(r.stop_detail.accuracy_stop==AccuracyStop::Conditioning)return FitOutcome::failure(OutcomeReason::Conditioning);
  switch(r.status) {
    case zero_detail::ModifiedStatus::Ok: return FitOutcome::estimate(!r.stop_detail.budget_exhausted);
    case zero_detail::ModifiedStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case zero_detail::ModifiedStatus::InvalidSize: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case zero_detail::ModifiedStatus::PoissonLimit: return FitOutcome::limit(true);
    case zero_detail::ModifiedStatus::AllZero: return FitOutcome::estimate(true,false);
    case zero_detail::ModifiedStatus::TwoPoint: return FitOutcome::estimate(true);
    case zero_detail::ModifiedStatus::IterationFailure:
      // At interior parent coefficients every positive count has positive
      // mass. A nonfinite point likelihood is an evaluation failure,
      // not a failed score solve or an incompatible point-mass endpoint.
      if(!std::isfinite(r.logLik) &&
         r.lambda>0 && std::isfinite(r.lambda) && r.size>0 && std::isfinite(r.size) &&
         r.prob>0 && r.prob<1 && r.p0>=0 && r.p0<1)
        return FitOutcome::failure(OutcomeReason::Evaluation);
      return FitOutcome::failure(OutcomeReason::NoConvergence);
  }
  return {};
}
} // namespace cppunivariate
