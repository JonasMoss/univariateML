#pragma once
#include "../ztpois/mle.hpp"
#include "../../core/stop_detail.hpp"
#include "../lgser/mle.hpp"
namespace cppunivariate {
enum class ZtnbinomStatus { Ok, InvalidInput, InvalidSize, PoissonLimit, IterationFailure };
enum class ZtnbinomCertificateFailure { None, Arithmetic, Budget, Workload, AdditionalExtrema };
struct ZtnbinomResult { double size, prob, logLik; int iterations; ZtnbinomStatus status; StopDetail stop_detail{}; bool budget_exhausted=false; ZtnbinomCertificateFailure certificate_failure=ZtnbinomCertificateFailure::None; };
inline ZtnbinomResult ztnbinom_candidate_mle(ispan x, double tol=1e-10, int maxit=200,
    double fixed_size=std::numeric_limits<double>::quiet_NaN(),
    double fixed_prob=std::numeric_limits<double>::quiet_NaN()) {
  const bool fixed=!std::isnan(fixed_size);
  if(fixed && (!(fixed_size>=0)||!std::isfinite(fixed_size)))
    return {0,0,0,0,ZtnbinomStatus::InvalidSize};
  if(!zero_detail::positive(x)||!(tol>0)||!std::isfinite(tol)||maxit<1)
    return {0,0,0,0,ZtnbinomStatus::InvalidInput};
  const double m=zero_detail::mean(x), n=x.size();
  if(!std::isnan(fixed_prob)) {
    if(!(fixed_prob>0 && fixed_prob<=1)||!std::isfinite(fixed_prob) ||
        (fixed_prob==1 && m>1)) return {0,0,0,0,ZtnbinomStatus::InvalidInput};
    auto evaluate=[&](double r) {
      double ll=0;
      if(fixed_prob==1) return ZtnbinomResult{r,fixed_prob,0,0,ZtnbinomStatus::Ok};
      for(int v:x) {
        if(r==0) ll+=v*std::log1p(-fixed_prob)-std::log(v)-std::log(-std::log(fixed_prob));
        else ll+=zero_detail::nb_logpmf(v,r,fixed_prob)-std::log(-std::expm1(r*std::log(fixed_prob)));
      }
      return ZtnbinomResult{r,fixed_prob,ll,0,ZtnbinomStatus::Ok};
    };
    if(fixed) return evaluate(fixed_size);
    if(fixed_prob==1) return evaluate(1);
    if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))
      return {0,0,0,0,ZtnbinomStatus::IterationFailure};
    auto best=evaluate(0);
    // The size-zero logarithmic member is attained. At infinity the
    // likelihood vanishes for held prob<1. Refine every grid peak.
    double upper=std::max(2.0,std::log(m*fixed_prob/(1-fixed_prob))+2);
    while(upper<700 && evaluate(std::exp(upper)).logLik>best.logLik-n) upper+=2;
    if(upper>=700) return {0,0,0,0,ZtnbinomStatus::IterationFailure};
    for(double t=-24;t<=upper;t+=0.5) {
      const auto mid=evaluate(std::exp(t));
      if(mid.logLik>best.logLik)best=mid;
      if(mid.logLik<evaluate(std::exp(t-.5)).logLik || mid.logLik<evaluate(std::exp(t+.5)).logLik)continue;
      const double opt=zero_detail::maximize([&](double a){return evaluate(std::exp(a)).logLik;},t-.5,t+.5,tol,maxit);
      const auto candidate=evaluate(std::exp(opt));
      if(candidate.logLik>best.logLik)best=candidate;
    }
    return best;
  }
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
  if(maxit < std::ceil(std::log(std::min(tol,0.5)/2)/std::log(0.6180339887498948482)))
    return {0,0,0,0,ZtnbinomStatus::IterationFailure};
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

#include "certificate.hpp"
