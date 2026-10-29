#pragma once
#include "../ztpois/mle.hpp"
#include "../lgser/mle.hpp"
namespace cppunivariate {
enum class ZtnbinomStatus { Ok, InvalidInput, InvalidSize, PoissonLimit, IterationFailure };
struct ZtnbinomResult { double size, prob, logLik; int iterations; ZtnbinomStatus status; };
inline ZtnbinomResult ztnbinom_mle(ispan x, double tol=1e-10, int maxit=200,
    double fixed_size=std::numeric_limits<double>::quiet_NaN()) {
  const bool fixed=!std::isnan(fixed_size);
  if(fixed && (!(fixed_size>=0)||!std::isfinite(fixed_size)))
    return {0,0,0,0,ZtnbinomStatus::InvalidSize};
  if(!zero_detail::positive(x)||!(tol>0)||!std::isfinite(tol)||maxit<1)
    return {0,0,0,0,ZtnbinomStatus::InvalidInput};
  const double m=zero_detail::mean(x), n=x.size();
  if(m==1) return {fixed?fixed_size:1,1,0,0,ZtnbinomStatus::Ok};
  const auto logarithmic=zero_detail::logarithmic_fit(x);
  auto profile=[&](double r) {
    if(r==0)return ZtnbinomResult{0,1-logarithmic.theta,logarithmic.logLik,0,ZtnbinomStatus::Ok};
    const double p=zero_detail::nb_prob(r,m);
    double ll=0;
    for(int v:x) ll+=zero_detail::nb_logpmf(v,r,p)-std::log(-std::expm1(r*std::log(p)));
    return ZtnbinomResult{r,p,ll,0,ZtnbinomStatus::Ok};
  };
  if(fixed) return profile(fixed_size);
  auto best=profile(0);
  // Include the attained logarithmic face and every bracketed local maximum.
  for(int t=-24;t<=24;++t) {
    auto mid=profile(std::exp(t));
    if(mid.logLik>best.logLik) best=mid;
    if(mid.logLik<profile(std::exp(t-1)).logLik || mid.logLik<profile(std::exp(t+1)).logLik) continue;
    const double opt=zero_detail::maximize([&](double s){return profile(std::exp(s)).logLik;},t-1,t+1,tol,maxit);
    auto candidate=profile(std::exp(opt));
    if(candidate.logLik>best.logLik) best=candidate;
  }
  const auto face=profile(0);
  if(face.logLik>=best.logLik-1e-8*n) best=face;
  const auto pois=ztpois_mle(x);
  if(pois.logLik>=best.logLik-1e-8*n) best.status=ZtnbinomStatus::PoissonLimit;
  if(best.status==ZtnbinomStatus::Ok && best.size>=std::exp(24.0))best.status=ZtnbinomStatus::IterationFailure;
  return best;
}
}
