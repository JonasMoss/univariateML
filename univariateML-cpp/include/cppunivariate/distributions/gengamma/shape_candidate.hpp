// One-dimensional shape-profile evaluations and local candidate refinement.
#pragma once
#include "held_joint.hpp"
#include "local.hpp"
#include "profile_root.hpp"

namespace cppunivariate {
namespace gengamma_fit_detail {

struct ShapePoint {
  double mu, sigma, q, mean_loglik, objective, score;
  bool valid;
};

inline ShapePoint shape_point(span y, double q, double mu, double sigma,
                              bool free_mu, bool free_sigma, double tol, int maxit) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const ShapePoint bad{nan,nan,q,nan,nan,nan,false};
  if (y.empty() || !std::isfinite(q) || !(tol>0) || !std::isfinite(tol) || maxit<1) return bad;
  const double inner_tol=std::max(tol*0.01,64*std::numeric_limits<double>::epsilon());
  if (free_mu && free_sigma) {
    const auto fit=location_scale(y,q,inner_tol,maxit);
    if (fit.status!=ConditionalStatus::Ok) return bad;
    mu=fit.mu;sigma=fit.sigma;
  } else if (free_mu) {
    const auto fit=location(y,sigma,q);
    if (fit.status!=ConditionalStatus::Ok) return bad;
    mu=fit.value;
  } else if (free_sigma) {
    const auto fit=scale(y,mu,q,inner_tol,maxit);
    if (fit.status!=ConditionalStatus::Ok) return bad;
    sigma=fit.value;
  }
  const auto score=shape_score(y,mu,sigma,q);
  if (!score.valid) return bad;
  CPPUNIV_EXTENDED_REAL sum=0, jacobian=0;
  for (double v:y) {
    const double w=(v-mu)/sigma;
    const double penalty=distributions::gengamma_detail::penalty(w,q);
    if (!std::isfinite(penalty)) return bad;
    sum-=static_cast<CPPUNIV_EXTENDED_REAL>(penalty);
    jacobian+=v;
  }
  const double objective=static_cast<double>(sum/y.size())-std::log(sigma)-
      distributions::gengamma_detail::half_log_two_pi-
      distributions::gengamma_detail::remainder(q);
  const double value=static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(objective)-jacobian/y.size());
  if (!std::isfinite(value) || !std::isfinite(objective)) return bad;
  return {mu,sigma,q,value,objective,score.value,true};
}

// Opposite profile-score signs nominate a local maximum. The separate
// positive-information Newton check must succeed before it is returned.
// The caller must still compare this candidate with all other candidates
// and reachable faces; the rounded log likelihood alone cannot settle a
// shallow comparison with Q=0.
inline ShapePoint shape_candidate(span y, ShapePoint left, ShapePoint right,
                                  bool free_mu, bool free_sigma,
                                  double center, double unit, double tol, int maxit) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const ShapePoint bad{nan,nan,nan,nan,nan,nan,false};
  if (!(left.valid && right.valid && left.q<right.q && left.score>0 && right.score<0 &&
        tol>0 && std::isfinite(tol) && maxit>0)) return bad;
  if (free_mu && free_sigma && ((left.q>0 && right.q>0) || (left.q<0 && right.q<0))) {
    const double sign=left.q>0 ? 1 : -1;
    std::vector<double> reflected;
    reflected.reserve(y.size());
    for (double v:y) reflected.push_back(sign*v);
    trgamma_detail::LogData d;
    if (trgamma_detail::standardize({reflected.data(),reflected.size()},d)) {
      const double a=std::abs(left.q)*d.sd/left.sigma;
      const double b=std::abs(right.q)*d.sd/right.sigma;
      const auto root=profile_root(d,std::min(a,b),std::max(a,b),tol,maxit);
      if (root.ok) {
        std::array<double,3> parameter{sign*root.mu,root.sigma,sign*root.q};
        if (polish_local(y,center,unit,parameter,{true,true,true},tol,maxit)) {
          const auto candidate=shape_point(y,parameter[2],parameter[0],parameter[1],false,false,tol,maxit);
          if (candidate.valid) return candidate;
        }
      }
    }
  }
  ShapePoint mid=left;
  const int budget=static_cast<int>(solver_budget(maxit));
  bool narrowed=false;
  for (int i=0;i<budget;++i) {
    const double q=left.q+(right.q-left.q)/2;
    if (!(q>left.q && q<right.q)) break;
    mid=shape_point(y,q,left.mu,left.sigma,free_mu,free_sigma,tol,maxit);
    if (!mid.valid) return bad;
    if (mid.score>0) left=mid;
    else if (mid.score<0) right=mid;
    else { narrowed=true;break; }
    if (right.q-left.q <= 0.05*tol*std::max(1.0,std::abs(q))) { narrowed=true;break; }
  }
  if (!narrowed) return bad;
  std::array<double,3> parameter{mid.mu,mid.sigma,mid.q};
  if (!polish_local(y,center,unit,parameter,{free_mu,free_sigma,true},tol,maxit)) return bad;
  return shape_point(y,parameter[2],parameter[0],parameter[1],false,false,tol,maxit);
}

}  // namespace gengamma_fit_detail
}  // namespace cppunivariate
