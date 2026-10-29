#pragma once
#include "../ztpois/mle.hpp"
namespace cppunivariate {
enum class ZtbinomStatus { Ok, InvalidInput, InvalidSize, PoissonLimit, IterationFailure };
struct ZtbinomResult { double size, prob, logLik; int iterations; ZtbinomStatus status; };
inline ZtbinomResult ztbinom_mle(ispan x, double tol=1e-10, int maxit=200,
    double fixed_size=std::numeric_limits<double>::quiet_NaN()) {
  const bool fixed=!std::isnan(fixed_size);
  if(fixed && (!(fixed_size>=1)||!std::isfinite(fixed_size)||fixed_size!=std::floor(fixed_size)))
    return {0,0,0,0,ZtbinomStatus::InvalidSize};
  if(!zero_detail::positive(x)||!(tol>0)||!std::isfinite(tol)||maxit<1)
    return {0,0,0,0,ZtbinomStatus::InvalidInput};
  int mx=1;for(int v:x) mx=std::max(mx,v);
  if(fixed && fixed_size<mx) return {0,0,0,0,ZtbinomStatus::InvalidSize};
  const double m=zero_detail::mean(x), n=x.size();
  if(m==1) return {fixed?fixed_size:1,0,0,0,ZtbinomStatus::Ok};
  auto profile=[&](double r) {
    const double p=zero_detail::bin_prob(r,m);
    double ll=0;
    for(int v:x) ll+=zero_detail::bin_logpmf(v,r,p)-std::log(-std::expm1(r*std::log1p(-p)));
    return ZtbinomResult{r,p,ll,0,ZtbinomStatus::Ok};
  };
  if(fixed) return profile(fixed_size);
  auto best=profile(mx);
  for(int t=0;t<=24;++t) {
    const double r=mx+std::exp(t);
    auto candidate=profile(std::floor(r));
    if(candidate.logLik>best.logLik)best=candidate;
    const double ll=profile(r).logLik;
    if(ll<profile(mx+std::exp(t-1)).logLik || ll<profile(mx+std::exp(t+1)).logLik)continue;
    const double opt=zero_detail::maximize([&](double s){return profile(mx+std::exp(s)).logLik;},t-1,t+1,tol,maxit);
    const double s=mx+std::exp(opt);
    for(double k:{std::floor(s),std::ceil(s)}) {candidate=profile(k);if(candidate.logLik>best.logLik)best=candidate;}
  }
  if(ztpois_mle(x).logLik>=best.logLik-1e-8*n)best.status=ZtbinomStatus::PoissonLimit;
  if(best.status==ZtbinomStatus::Ok && best.size>=std::floor(mx+std::exp(24.0)))best.status=ZtbinomStatus::IterationFailure;
  return best;
}
}
