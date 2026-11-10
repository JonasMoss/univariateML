// Inverse-gamma sufficient statistics are formed from raw reciprocal
// differences; the public scale is the gamma rate on 1/X.

#pragma once
#include "../../core/work_budget.hpp"

#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include "../gamma/transformed.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct InvGammaResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  GammaStatus status = GammaStatus::Ok;
  bool certified=true;
};

inline InvGammaResult invgamma_mle(span x,double tol,int maxit,double shape,double scale) {
  const auto g=transformed_detail::gamma_raw(x,tol,maxit,shape,scale,transformed_detail::GammaMap::Reciprocal);
  return {g.shape,g.scale,g.logLik,g.iterations,g.status,g.certified};
}
inline InvGammaResult invgamma_mle(span x,double tol,int maxit) {return invgamma_mle(x,tol,maxit,NAN,NAN);}
} // namespace cppunivariate
