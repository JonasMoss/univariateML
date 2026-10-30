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

struct CmbSizeResult { double size; CmbResult fit; };
// Both canonical coordinates held. Enumerate all integers, and bound the
// entire remaining tail by a single normalizer term. This does not assume
// unimodality in size (which is not implied by exponential-family concavity).
inline CmbSizeResult cmb_size_mle(ispan x,double prob,double nu,
                                 double tol=1e-10,int maxit=100) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto fail=[&](CmbStatus s) {return CmbSizeResult{nan,{prob,nu,nan,0,s}};};
  if(x.size()==0 || !std::isfinite(prob)||prob<0||prob>1||!std::isfinite(nu)||
     !(tol>0)||!std::isfinite(tol)||maxit<1)return fail(CmbStatus::InvalidInput);
  int mx=0;double sx=0;
  for(int v:x){if(v<0)return fail(CmbStatus::InvalidInput);mx=std::max(mx,v);sx+=v;}
  if(prob==0 || prob==1) {
    for(int v:x)if(v!=(prob==0 ? 0 : mx))return fail(CmbStatus::InvalidInput);
    return {double(mx),{prob,nu,0,0,CmbStatus::Ok}};
  }
  if(mx==0)return {0,{prob,nu,0,0,CmbStatus::Ok}};
  const double eta=std::log(prob)-std::log1p(-prob),n=double(x.size());
  CmbSizeResult best{nan,{prob,nu,-INFINITY,0,CmbStatus::IterationFailure}};
  // Work budget counts enumerated normalizer terms, not size magnitude.
  // Table also computes moments and tails, so charge its full support.
  std::size_t work=0;
  for(int N=mx;;++N) {
    if(static_cast<std::size_t>(N)+1>4000000-work)return fail(CmbStatus::IterationFailure);
    work+=static_cast<std::size_t>(N)+1;
    const auto f=cmb_mle(x,N,tol,maxit,prob,nu);
    if(f.status!=CmbStatus::Ok)return {double(N),f};
    if(!std::isfinite(f.logLik))return fail(CmbStatus::IterationFailure);
    if(f.logLik>best.fit.logLik)best={double(N),f};
    // nu=0: weights r^x are independent of size and the normalizer strictly
    // increases, so the first feasible size is the exact optimum.
    if(nu==0)return best;
    double upper=0;
    if(nu>0) {
      const double next=double(N)+1;
      const int k=mx+1;
      for(int v:x) {
        double ratio=0;
        for(int j=v;j<k;++j)ratio+=std::log((j+1)/(next-j));
        upper+=nu*ratio+(v-double(k))*eta;
      }
      // C(M,v)/C(M,k) decreases in M for v<k; one term of Z
      // gives this bound simultaneously for every M>=N+1.
    } else {
      // Z>=max(1,exp(M*eta)); every positive logchoose(M,v)
      // increases in M, while nu<0. Thus this bound also decreases.
      for(int v:x)upper+=nu*cmb_detail::logchoose(N+1,v);
      upper+=eta*sx-n*std::max(0.,(N+1.)*eta);
    }
    if(upper<best.fit.logLik-1e-10*n)return best;
  }
}
}
