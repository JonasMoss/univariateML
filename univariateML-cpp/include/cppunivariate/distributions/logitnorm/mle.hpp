// Logit-normal MLE: y = logit(x), then Normal MLE on y. Adjusted log-
// likelihood includes the change-of-variable Jacobian -sum(log(x*(1-x))).

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

enum class LogitnormStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct LogitnormResult {
  double mu;
  double sigma;
  double logLik;
  LogitnormStatus status = LogitnormStatus::Ok;
  bool certified=true;
};

inline FitOutcome outcome(const LogitnormResult& r) {
  switch (r.status) {
    case LogitnormStatus::Ok: return FitOutcome::estimate(r.certified);
    case LogitnormStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case LogitnormStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case LogitnormStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}

inline LogitnormResult logitnorm_mle(span x,
    double mu = std::numeric_limits<double>::quiet_NaN(),
    double sigma = std::numeric_limits<double>::quiet_NaN()) {
  const auto r=transformed_detail::normal(transformed_detail::logs(x,true),mu,sigma);
  if(r.status==2 && (!std::isnan(mu) || !std::isnan(sigma)))
    return {NAN,NAN,NAN,LogitnormStatus::Unbounded};
  return {r.mean,r.sd,r.ll,r.status==0?LogitnormStatus::Ok:
    r.status==1?LogitnormStatus::InvalidInput:r.status==2?LogitnormStatus::Unbounded:LogitnormStatus::IterationFailure,r.certified};
}
} // namespace cppunivariate
