#pragma once
#include "../ztpois/mle.hpp"
namespace cppunivariate {
enum class ZtbinomStatus { Ok, InvalidInput, InvalidSize, PoissonLimit, IterationFailure };
struct ZtbinomResult { double size, prob, logLik; int iterations; ZtbinomStatus status; };
namespace ztbinom_detail {
template <typename F>
inline double maximize(F f, double lo, double hi, double tol, int maxit, bool &done) {
  constexpr double ratio=.6180339887498948482;
  double c=hi-ratio*(hi-lo), d=lo+ratio*(hi-lo), fc=f(c), fd=f(d);
  for(int i=0;i<maxit && hi-lo>tol;++i) {
    if(fc>fd) {hi=d;d=c;fd=fc;c=hi-ratio*(hi-lo);fc=f(c);}
    else {lo=c;c=d;fc=fd;d=lo+ratio*(hi-lo);fd=f(d);}
  }
  done=hi-lo<=tol && std::isfinite(fc) && std::isfinite(fd);
  return fc>fd ? c : d;
}
}
inline ZtbinomResult ztbinom_mle(ispan x, double tol=1e-10, int maxit=200,
    double fixed_size=std::numeric_limits<double>::quiet_NaN(),
    double fixed_prob=std::numeric_limits<double>::quiet_NaN()) {
  const bool fixed=!std::isnan(fixed_size);
  if(fixed && (!(fixed_size>=1)||!std::isfinite(fixed_size)||fixed_size!=std::floor(fixed_size)))
    return {0,0,0,0,ZtbinomStatus::InvalidSize};
  if(!zero_detail::positive(x)||!(tol>0)||!std::isfinite(tol)||maxit<1)
    return {0,0,0,0,ZtbinomStatus::InvalidInput};
  int mx=1;for(int v:x) mx=std::max(mx,v);
  if(fixed && fixed_size<mx) return {0,0,0,0,ZtbinomStatus::InvalidSize};
  const double m=zero_detail::mean(x), n=x.size();
  if (!std::isnan(fixed_prob)) {
    const double p=fixed_prob;
    if (!(p>=0 && p<=1) || !std::isfinite(p)) return {0,0,0,0,ZtbinomStatus::InvalidInput};
    const auto profile_held=[&](double size) {
      double ll=0;
      for(int v:x) {
        if(p==0) {if(v!=1)ll=-INFINITY;}
        else if(p==1) {if(v!=size)ll=-INFINITY;}
        else ll+=zero_detail::bin_logpmf(v,size,p)-std::log(-std::expm1(size*std::log1p(-p)));
      }
      return ZtbinomResult{size,p,ll,0,std::isfinite(ll)?ZtbinomStatus::Ok:ZtbinomStatus::InvalidInput};
    };
    if(fixed)return profile_held(fixed_size);
    if(p==0 || p==1)return profile_held(mx);
    auto best=profile_held(mx);
    for(int j=0;j<1000000;++j) {
      const double size=double(mx)+j;
      auto candidate=profile_held(size);
      if(candidate.logLik>best.logLik)best=candidate;
      // For size >= mx/p every mass on the observed support decreases.
      // The truncation probability increases, so this bounds all future sizes.
      if(size>=mx/p) {
        const double upper=n*(zero_detail::bin_logpmf(mx,size,p)-std::log(-std::expm1(size*std::log1p(-p))));
        if(upper<best.logLik) return best;
      }
    }
    best.status=ZtbinomStatus::IterationFailure;return best;
  }
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
    bool done=false;
    const double opt=ztbinom_detail::maximize([&](double s){return profile(mx+std::exp(s)).logLik;},t-1,t+1,tol,maxit,done);
    if(!done) {best.status=ZtbinomStatus::IterationFailure;return best;}
    const double s=mx+std::exp(opt);
    for(double k:{std::floor(s),std::ceil(s)}) {candidate=profile(k);if(candidate.logLik>best.logLik)best=candidate;}
  }
  if(ztpois_mle(x).logLik>=best.logLik-1e-8*n)best.status=ZtbinomStatus::PoissonLimit;
  if(best.status==ZtbinomStatus::Ok && best.size>=std::floor(mx+std::exp(24.0)))best.status=ZtbinomStatus::IterationFailure;
  return best;
}
}
