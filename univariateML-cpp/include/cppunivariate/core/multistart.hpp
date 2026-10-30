#pragma once
#include "bfgs.hpp"
#include <cmath>
#include <algorithm>
#include <vector>

namespace cppunivariate {
// R6: failed solves need a finite TOTAL score in free fitter coordinates.
inline bool bfgs_candidate_acceptable(const BfgsResult& r, double divisor=1.0) {
  if (!std::isfinite(r.f) || r.status==BfgsStatus::BadInit || r.theta.empty() ||
      r.grad.size()!=r.theta.size()) return false;
  double norm=0;
  for (std::size_t j=0;j<r.theta.size();++j) {
    if (!std::isfinite(r.theta[j]) || !std::isfinite(r.grad[j])) return false;
    norm=std::hypot(norm,r.grad[j]*divisor);
  }
  return std::isfinite(norm) && (r.status==BfgsStatus::Ok || norm<1e-3);
}
// Replace best with r when r is acceptable and better; an accepted failed
// solve (score gate) is promoted to Ok.
inline bool consider_bfgs_candidate(BfgsResult& best, BfgsResult r, double divisor=1.0) {
  if (!bfgs_candidate_acceptable(r,divisor) || !(r.f<best.f)) return false;
  r.status=BfgsStatus::Ok;
  best=std::move(r);
  return true;
}
// As above, and also track the best finite attempted point, acceptable or not.
// It is kept only for failure diagnostics and the family's existing comparison
// to independently fitted limit models.
inline bool consider_bfgs_candidate(BfgsResult& best, BfgsResult r, double divisor,
                                    BfgsResult& attempted) {
  bool finite=std::isfinite(r.f) && !r.theta.empty();
  for(double t:r.theta) finite=finite && std::isfinite(t);
  if(finite && r.f<attempted.f) attempted=r;
  return consider_bfgs_candidate(best,std::move(r),divisor);
}
// For non-BFGS solves, differentiate the total log likelihood, varying only
// free coordinates. Callers supply the actual likelihood, never a face bound.
template <typename Value>
inline bool multistart_score_acceptable(std::vector<double> theta, Value value) {
  if (!std::isfinite(value(theta))) return false;
  double norm=0;
  for (std::size_t j=0;j<theta.size();++j) {
    const double t=theta[j], h=1.4901161193847656e-8*std::max(1.0,std::abs(t));
    if (!std::isfinite(t)) return false;
    theta[j]=t+h; const double hi=value(theta);
    theta[j]=t-h; const double lo=value(theta);
    theta[j]=t;
    norm=std::hypot(norm,(hi-lo)/(2*h));
  }
  return std::isfinite(norm) && norm<1e-3;
}
}
