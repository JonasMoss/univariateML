// Inverse-Weibull fitting brackets the Gumbel profile on centered raw
// log ratios, with a checked restoration of the public scale.

#pragma once
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

// A starting shape is retained in the API; the bracketed profile does not
// need it. A nonpositive supplied start remains invalid input.
inline InvWeibullResult invweibull_mle(span x,double tol,int maxit,double shape0,double shape,double scale) {
  if(!std::isnan(shape0) && !(shape0>0 && std::isfinite(shape0)))return {NAN,NAN,NAN,0,InvWeibullStatus::InvalidInput};
  const auto r=transformed_detail::location_fit(x,shape,scale,1,true,1,tol,maxit);
  return {r.shape,r.scale,r.ll,0,r.status==0?InvWeibullStatus::Ok:
    r.status==1?InvWeibullStatus::InvalidInput:r.status==2?InvWeibullStatus::Unbounded:InvWeibullStatus::IterationFailure,r.certified};
}
inline InvWeibullResult invweibull_mle(span x,double tol,int maxit,double shape0=NAN) {
  return invweibull_mle(x,tol,maxit,shape0,NAN,NAN);
}
} // namespace cppunivariate
