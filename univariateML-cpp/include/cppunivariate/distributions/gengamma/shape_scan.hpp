// Candidate collection on the two signed shape profiles.
#pragma once
#include "shape_candidate.hpp"
#include <vector>

namespace cppunivariate {
namespace gengamma_fit_detail {

struct ShapeCandidates {
  std::vector<ShapePoint> values;
  int evaluations=0;
  bool scan_completed=false;
};

// The caller supplies the scanned range and remains responsible for the
// intervals between evaluations, omitted seam, tails, all candidate/face
// comparisons and global status. A completed scan is not a certificate.
// A successful local candidate remains usable as an uncertified proposal
// when this cover cannot be completed.
inline ShapeCandidates shape_candidates(span y, double mu, double sigma,
                                         bool free_mu, bool free_sigma,
                                         double center, double unit,
                                         double qmin, double qmax,
                                         double tol, int maxit,
                                         std::int64_t budget_override=-1) {
  ShapeCandidates out;
  if (!(qmin>0 && qmax>qmin && std::isfinite(qmax) && tol>0 &&
        std::isfinite(tol) && maxit>0)) return out;
  const std::int64_t budget=budget_override<0 ? certificate_budget(maxit) :
      std::min(budget_override,certificate_budget(maxit));
  if (budget<1) return out;
  const auto zero=shape_point(y,0,mu,sigma,free_mu,free_sigma,tol,maxit);
  ++out.evaluations;
  if (!zero.valid) return out;
  std::array<double,3> start{zero.mu,zero.sigma,0};
  if (polish_local(y,center,unit,start,{free_mu,free_sigma,true},tol,maxit)) {
    const auto candidate=shape_point(y,start[2],start[0],start[1],false,false,tol,maxit);
    if (candidate.valid) out.values.push_back(candidate);
  }
  ShapePoint prev_pos=zero, prev_neg=zero;
  double q=qmin;
  bool all_valid=true;
  while (out.evaluations+2<=budget) {
    const auto pos=shape_point(y,q,zero.mu,zero.sigma,free_mu,free_sigma,tol,maxit);
    const auto neg=shape_point(y,-q,zero.mu,zero.sigma,free_mu,free_sigma,tol,maxit);
    out.evaluations+=2;
    if (prev_pos.valid && pos.valid && prev_pos.score>0 && pos.score<0) {
      const auto candidate=shape_candidate(y,prev_pos,pos,free_mu,free_sigma,center,unit,tol,maxit);
      if (candidate.valid) out.values.push_back(candidate); else all_valid=false;
    }
    if (prev_neg.valid && neg.valid && neg.score>0 && prev_neg.score<0) {
      const auto candidate=shape_candidate(y,neg,prev_neg,free_mu,free_sigma,center,unit,tol,maxit);
      if (candidate.valid) out.values.push_back(candidate); else all_valid=false;
    }
    if (!pos.valid || !neg.valid) all_valid=false;
    prev_pos=pos;prev_neg=neg;
    if (q==qmax) {out.scan_completed=all_valid;break;}
    const double next=q<=qmax/2 ? q*2 : qmax;
    if (!(next>q)) break;
    q=next;
  }
  return out;
}

}  // namespace gengamma_fit_detail
}  // namespace cppunivariate
