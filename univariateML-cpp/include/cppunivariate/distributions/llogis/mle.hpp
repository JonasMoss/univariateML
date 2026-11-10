// Log-logistic fitting brackets the strictly concave logistic profile
// in centered raw log ratios, then checks the restored public scale.

#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../../core/transformed_location.hpp"
#include "../logis/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class LogLogisticStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct LogLogisticResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  LogLogisticStatus status = LogLogisticStatus::Ok;
  bool certified=true;
};

inline LogLogisticResult loglogistic_mle(span x,double tol,int maxit,double shape,double scale) {
  const auto r=transformed_detail::location_fit(x,shape,scale,1,false,1,tol,maxit);
  return {r.shape,r.scale,r.ll,0,r.status==0?LogLogisticStatus::Ok:
    r.status==1?LogLogisticStatus::InvalidInput:r.status==2?LogLogisticStatus::Unbounded:LogLogisticStatus::IterationFailure,r.certified};
}
inline LogLogisticResult loglogistic_mle(span x,double tol,int maxit) {return loglogistic_mle(x,tol,maxit,NAN,NAN);}
} // namespace cppunivariate
