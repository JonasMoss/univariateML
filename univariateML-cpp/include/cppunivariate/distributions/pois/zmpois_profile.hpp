// Compatibility forwarding header; implementation lives in core.
#pragma once
#include "../../core/outcome.hpp"
#include "../../core/zero_modified_fit.hpp"

namespace cppunivariate {
// ZmpoisResult, ZmgeomResult and ZmlogarithmicResult are aliases of this type.
// Keep their single overload in this shared family forwarding header.
inline FitOutcome outcome(const zero_detail::ModifiedResult& r) {
  switch(r.status) {
    case zero_detail::ModifiedStatus::Ok: return FitOutcome::estimate(true);
    case zero_detail::ModifiedStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case zero_detail::ModifiedStatus::InvalidSize: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case zero_detail::ModifiedStatus::PoissonLimit: return FitOutcome::limit(true);
    case zero_detail::ModifiedStatus::AllZero: return FitOutcome::estimate(true);
    case zero_detail::ModifiedStatus::TwoPoint: return FitOutcome::estimate(true);
    case zero_detail::ModifiedStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
  }
  return {};
}
} // namespace cppunivariate
