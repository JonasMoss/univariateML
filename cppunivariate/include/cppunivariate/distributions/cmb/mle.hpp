// Full finite exponential family in (logit(prob), nu).
#pragma once
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <limits>
namespace cppunivariate {
enum class CmbStatus { Ok, InvalidInput, PointMass, AdjacentLimit, EndpointLimit,
                       Unidentified, IterationFailure };
struct CmbResult { double prob,nu,logLik; int iterations; CmbStatus status; };
namespace cmb_detail {
inline double probability(double eta) {
  return eta>=0 ? 1/(1+std::exp(-eta)) : std::exp(eta)/(1+std::exp(eta));
}
// Mean matching has derivative Var(X). Safeguarded Newton within a bracket.
inline double match_mean(int N,double nu,double m) {
  double lo=-1,hi=1;
  for (int j=0;j<60 && Table(N,lo,nu).mean>m;++j) lo*=2;
  for (int j=0;j<60 && Table(N,hi,nu).mean<m;++j) hi*=2;
  double eta=0;
  for (int j=0;j<100;++j) {
    const Table tab(N,eta,nu);
    const double score=tab.mean-m;
    if (std::abs(score)<2e-14*(1+m)) return eta;
    if (score<0) lo=eta; else hi=eta;
    const double next=eta-score/tab.vx;
    eta=std::isfinite(next) && next>lo && next<hi ? next : lo+(hi-lo)/2;
  }
  return eta;
}
}
inline CmbResult cmb_mle(ispan x,int N,double tol=1e-10,int maxit=100,
                        double prob=std::numeric_limits<double>::quiet_NaN(),
                        double nu=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool fp=!std::isnan(prob),fn=!std::isnan(nu);
  const auto fail=[&](CmbStatus status) { return CmbResult{nan,nan,nan,0,status}; };
  if (x.size()==0 || N<0 || N>10000000 || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
      (fp && !(prob>=0 && prob<=1)) || (fn && !std::isfinite(nu))) return fail(CmbStatus::InvalidInput);
  int xmin=N,xmax=0; double sx=0,sh=0;
  bool endpoints=true,centres=true;
  for (int v:x) {
    if (v<0 || v>N) return fail(CmbStatus::InvalidInput);
    xmin=std::min(xmin,v); xmax=std::max(xmax,v); sx+=v; sh+=cmb_detail::logchoose(N,v);
    endpoints=endpoints && (v==0 || v==N);
    centres=centres && (v==N/2 || v==(N+1)/2);
  }
  const double n=static_cast<double>(x.size()),m=sx/n,t=sh/n;
  if (N==0) return {fp?prob:.5,fn?nu:1,0,0,CmbStatus::Ok};
  if (fp && (prob==0 || prob==1))
    return (prob==0?xmax==0:xmin==N) ? CmbResult{prob,fn?nu:1,0,0,CmbStatus::Ok} : fail(CmbStatus::InvalidInput);
  if (!fp && (m==0 || m==N)) return {m==0?0.0:1.0,fn?nu:1,0,0,CmbStatus::Ok};
  if (!fp && !fn && N>1) {
    if (xmin==xmax) return fail(CmbStatus::PointMass);
    if (xmax-xmin==1) return fail(CmbStatus::AdjacentLimit);
    if (endpoints) return fail(CmbStatus::EndpointLimit);
  }
  if (fp && !fn && N>1) {
    if (endpoints) return fail(CmbStatus::EndpointLimit);
    if (centres) return fail(N%2==0 ? CmbStatus::PointMass : CmbStatus::AdjacentLimit);
  }
  const double fixed_eta=fp ? std::log(prob)-std::log1p(-prob) : 0;
  int iterations=0;
  if (!fn && N>1) {
    const auto score=[&](double v) {
      const double eta=fp ? fixed_eta : cmb_detail::match_mean(N,v,m);
      return cmb_detail::Table(N,eta,v).mh-t;
    };
    // Profile derivative Var(h)-Cov(X,h)^2/Var(X) is positive.
    double lo=-1,hi=1;
    for (int j=0;j<50 && score(lo)>0;++j) lo*=2;
    for (int j=0;j<50 && score(hi)<0;++j) hi*=2;
    if (!(score(lo)<=0 && score(hi)>=0)) return fail(CmbStatus::IterationFailure);
    for (;iterations<maxit;++iterations) {
      nu=lo+(hi-lo)/2;
      const double g=score(nu);
      if (std::abs(g)<tol*(1+std::abs(t))) break;
      if (g<0) lo=nu; else hi=nu;
    }
    if (iterations==maxit) return fail(CmbStatus::IterationFailure);
  } else if (!fn) nu=1;
  const double eta=fp ? fixed_eta : cmb_detail::match_mean(N,nu,m);
  const double fitted_prob=fp ? prob : cmb_detail::probability(eta);
  if (!(fitted_prob>0 && fitted_prob<1)) return fail(CmbStatus::IterationFailure);
  // Evaluate the coordinates actually returned to the caller, including
  // the rounding incurred by converting canonical log odds to probability.
  const double reported_eta=std::log(fitted_prob)-std::log1p(-fitted_prob);
  const auto tab=cmb_detail::Table(N,reported_eta,nu);
  if ((!fp && std::abs(tab.mean-m)>1e-10*(1+m)) ||
      (!fn && N>1 && std::abs(tab.mh-t)>tol*(1+std::abs(t))))
    return fail(CmbStatus::IterationFailure);
  double ll=0; for (int v:x) ll+=tab.lp[v];
  return {fitted_prob,nu,ll,iterations,CmbStatus::Ok};
}
}
