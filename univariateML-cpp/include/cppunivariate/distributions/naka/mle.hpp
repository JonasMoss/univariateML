// Nakagami fitting uses gamma sufficient statistics of X^2, retaining
// relative squared differences and restoring the public second moment.

#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include "../gamma/transformed.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct NakaResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  GammaStatus status = GammaStatus::Ok;
  bool certified=true;
};

inline FitOutcome outcome(const NakaResult& r) {
  switch (r.status) {
    case GammaStatus::Ok: return FitOutcome::estimate(r.certified);
    case GammaStatus::Degenerate: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case GammaStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case GammaStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case GammaStatus::TransformFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}

inline NakaResult naka_mle(span x,double tol,int maxit,double shape,double scale) {
  const auto g=transformed_detail::gamma_raw(x,tol,maxit,shape,scale,transformed_detail::GammaMap::Square);
  return {g.shape,g.scale,g.logLik,g.iterations,g.status,g.certified};
}
inline NakaResult naka_mle(span x,double tol,int maxit) {return naka_mle(x,tol,maxit,NAN,NAN);}
} // namespace cppunivariate
