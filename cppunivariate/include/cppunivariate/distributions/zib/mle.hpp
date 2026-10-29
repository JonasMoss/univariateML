#pragma once
#include "../ztbinom/mle.hpp"
#include "../zip/mle.hpp"
namespace cppunivariate {
enum class ZibStatus { Ok, InvalidInput, InvalidSize, ZipLimit, IterationFailure };
struct ZibResult {double size, prob, pi, logLik;int iterations;ZibStatus status;};
inline ZibResult zib_mle(ispan x,double tol=1e-10,int maxit=200,
    double fixed_size=std::numeric_limits<double>::quiet_NaN()) {
  const bool fixed=!std::isnan(fixed_size);
  if(fixed && (!(fixed_size>=0)||!std::isfinite(fixed_size)||fixed_size!=std::floor(fixed_size)))
    return {0,0,0,0,0,ZibStatus::InvalidSize};
  if(x.size()==0||!(tol>0)||!std::isfinite(tol)||maxit<1)return {0,0,0,0,0,ZibStatus::InvalidInput};
  double sum=0,zeros=0;int mx=0;
  for(int v:x) {if(v<0)return {0,0,0,0,0,ZibStatus::InvalidInput};sum+=v;zeros+=v==0;mx=std::max(mx,v);}
  if(fixed && fixed_size<mx)return {0,0,0,0,0,ZibStatus::InvalidSize};
  if(sum==0)return {fixed?fixed_size:1,0,0,0,0,ZibStatus::Ok};
  const double n=x.size(), z=zeros/n, m=sum/n, pm=sum/(n-zeros);
  auto profile=[&](double r) {
    double p=zero_detail::bin_prob(r,pm);
    // If the unconstrained mixture needs negative inflation, pi=0.
    if(std::exp(r*std::log1p(-p))>z)p=m/r;
    const double logf0=r*std::log1p(-p), f0=std::exp(logf0);
    const double pi=f0<z ? (z-f0)/(-std::expm1(logf0)) : 0;
    double ll=0;
    for(int v:x) {
      if(v==0)ll+=std::log(pi+(1-pi)*f0);
      else ll+=std::log1p(-pi)+zero_detail::bin_logpmf(v,r,p);
    }
    return ZibResult{r,p,pi,ll,0,ZibStatus::Ok};
  };
  if(fixed)return profile(fixed_size);
  auto best=profile(mx);
  for(int t=0;t<=24;++t) {
    const double r=mx+std::exp(t),ll=profile(r).logLik;
    auto candidate=profile(std::floor(r));if(candidate.logLik>best.logLik)best=candidate;
    if(ll<profile(mx+std::exp(t-1)).logLik||ll<profile(mx+std::exp(t+1)).logLik)continue;
    const double opt=zero_detail::maximize([&](double s){return profile(mx+std::exp(s)).logLik;},t-1,t+1,tol,maxit);
    const double s=mx+std::exp(opt);
    for(double k:{std::floor(s),std::ceil(s)}) {candidate=profile(k);if(candidate.logLik>best.logLik)best=candidate;}
  }
  const auto zip=zip_mle(x);
  if(zip.status==ZipStatus::Ok && zip.logLik>=best.logLik-1e-8*n)best.status=ZibStatus::ZipLimit;
  if(best.status==ZibStatus::Ok && best.size>=std::floor(mx+std::exp(24.0)))best.status=ZibStatus::IterationFailure;
  return best;
}
}
