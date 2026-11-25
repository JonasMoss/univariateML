#pragma once
#include "../../core/outcome.hpp"
#include "../../core/precision.hpp"
#include "ztpois_profile.hpp"
#include "ztpois_dpqr.hpp"
namespace cppunivariate {
namespace zero_detail {
struct PositiveRoot { double coefficient; int iterations; bool closed; double error=0; };
// Mean equations for the positive conditional laws are strictly increasing.
// Only move an endpoint when the residual clears its arithmetic margin.
template<class Score>
inline PositiveRoot positive_root(double hi,double tol,double scale,int maxit,Score sign,double target=INFINITY) {
  double lo=0;
  int i=0;
  for(;i<solver_budget(maxit);++i) {
    const double mid=lo+(hi-lo)/2;
    if(hi-lo<=std::min(tol,target)*std::max(scale,lo) || mid==lo || mid==hi)break;
    const int at=sign(mid,true);
    if(at<0)lo=mid;
    else if(at>0)hi=mid;
    else {
      const double l=lo+(mid-lo)/2,h=mid+(hi-mid)/2;
      const bool below=sign(l,true)<0,above=sign(h,true)>0;
      if(!below && !above)break;
      if(below)lo=l;
      if(above)hi=h;
    }
  }
  const bool closed=hi-lo<=tol*std::max(scale,lo);
  double l=lo,h=hi;
  // An optional finer representative stays inside the confirmed bounds.
  if(closed)for(;i<solver_budget(maxit);++i) {
    const double mid=l+(h-l)/2;
    if(mid==l || mid==h)break;
    if(sign(mid,false)<0)l=mid;else h=mid;
  }
  const double mid=l+(h-l)/2;
  return {mid,i,closed,std::max(mid-lo,hi-mid)};
}
inline PositiveRoot poisson_positive_root(ispan x,double tol,int maxit,double scale=1,double target=INFINITY) {
  CPPUNIV_EXTENDED_REAL excess=0;
  for(int v:x)excess+=v-1;
  excess/=x.size();
  if(excess==0)return {0,0,true};
  return positive_root(static_cast<double>(1+excess)*2,tol,scale,maxit,[&](double p,bool confirmed) {
    if(!confirmed)return p/(-std::expm1(-p))<static_cast<double>(1+excess)?-1:1;
    const CPPUNIV_EXTENDED_REAL t=p;
    // Evaluate the excess over one without subtracting near-equal values.
    const auto den=-std::expm1(-t);
    const auto v=t<.01 ? t*(.5+t*(1./12+t*t*(-1./720+t*t/30240))) : (t-den)/den;
    const auto pad=16*std::numeric_limits<CPPUNIV_EXTENDED_REAL>::epsilon()*
      (t<.01?v:1+v+excess)+(t<.01?std::pow(t,8)/1e6:0);
    return v<excess-pad?-1:v>excess+pad?1:0;
  },target);
}

inline PositiveRoot logarithmic_positive_root(ispan x,double tol,int maxit,bool complement=false) {
  CPPUNIV_EXTENDED_REAL excess=0;
  for(int v:x)excess+=v-1;
  excess/=x.size();
  if(excess==0)return {complement?1.:0.,0,true};
  return positive_root(1,tol,complement?0:1,maxit,[&](double p,bool confirmed) {
    const CPPUNIV_EXTENDED_REAL t=complement?1-static_cast<CPPUNIV_EXTENDED_REAL>(p):p;
    const auto den=complement?-p*std::log(static_cast<CPPUNIV_EXTENDED_REAL>(p)):
      -(1-t)*std::log1p(-t);
    const auto v=t<.0001 ? t*(.5+t*(5./12+t*(3./8+t*251./720))) : (t-den)/den;
    const auto pad=16*std::numeric_limits<CPPUNIV_EXTENDED_REAL>::epsilon()*
      (t<.0001?v:1+v+excess)+(t<.0001?std::pow(t,5):0);
    const auto margin=confirmed?pad:0;
    const int sign=v<excess-margin?-1:v>excess+margin?1:0;
    return complement?-sign:sign;
  },1e-13);
}
}

enum class ZtpoisStatus { Ok, InvalidInput, NumericalFailure, IterationFailure };
struct ZtpoisResult { double lambda, logLik; int iterations; ZtpoisStatus status; StopDetail stop_detail{}; };
inline ZtpoisResult ztpois_mle(ispan x, double fixed=std::numeric_limits<double>::quiet_NaN(),
    double tol=1e-10,int maxit=200) {
  if(!zero_detail::positive(x) || !(tol>0 && std::isfinite(tol)) || maxit<1) return {0,0,0,ZtpoisStatus::InvalidInput};
  if(!std::isnan(fixed)) {
    if(!(fixed>=0 && std::isfinite(fixed)))return {0,0,0,ZtpoisStatus::InvalidInput};
    double ll=0;for(int v:x)ll+=distributions::dztpois(v,fixed,true);
    if(!std::isfinite(ll))return {fixed,ll,0,fixed==0?ZtpoisStatus::InvalidInput:ZtpoisStatus::NumericalFailure};
    return {fixed,ll,0,ZtpoisStatus::Ok};
  }
  const auto root=zero_detail::poisson_positive_root(x,tol,maxit,1,1e-13);
  double ll=0;
  for(int v:x)ll+=distributions::dztpois(v,root.coefficient,true);
  ZtpoisResult out{root.coefficient,ll,root.iterations,
    !std::isfinite(ll)?ZtpoisStatus::NumericalFailure:
    root.closed?ZtpoisStatus::Ok:ZtpoisStatus::IterationFailure};
  if(out.status==ZtpoisStatus::IterationFailure)
    record_accuracy(out.stop_detail,tol,{out.lambda},ll,{std::max(1.,out.lambda)});
  return out;
}
}

namespace cppunivariate {
inline FitOutcome outcome(const ZtpoisResult& r) {
  if(r.stop_detail.accuracy_stop==AccuracyStop::Tolerance)return FitOutcome::failure(OutcomeReason::Tolerance);
  if(r.stop_detail.accuracy_stop==AccuracyStop::Conditioning)return FitOutcome::failure(OutcomeReason::Conditioning);
  switch(r.status) {
    case ZtpoisStatus::Ok: return FitOutcome::singleton(true);
    case ZtpoisStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case ZtpoisStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case ZtpoisStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
} // namespace cppunivariate
