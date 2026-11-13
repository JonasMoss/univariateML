#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../pois/ztpois_mle.hpp"
#include "../../core/stop_detail.hpp"
#include "../lgser/mle.hpp"
namespace cppunivariate {
enum class ZtnbinomStatus { Ok, InvalidInput, InvalidSize, PoissonLimit, IterationFailure };
enum class ZtnbinomCertificateFailure { None, Arithmetic, Budget, Workload, AdditionalExtrema };
struct ZtnbinomResult { double size, prob, logLik; int iterations; ZtnbinomStatus status; StopDetail stop_detail{}; bool budget_exhausted=false; ZtnbinomCertificateFailure certificate_failure=ZtnbinomCertificateFailure::None; };
inline ZtnbinomResult ztnbinom_candidate_mle(ispan x, double tol=1e-10, int maxit=200,
    double fixed_size=std::numeric_limits<double>::quiet_NaN(),
    double fixed_prob=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
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
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (upper<700 && evaluate(std::exp(upper)).logLik>best.logLik-n); ++bracket_pass) upper+=2;
    if(upper>=700 || evaluate(std::exp(upper)).logLik>best.logLik-n) return {0,0,0,0,ZtnbinomStatus::IterationFailure};
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
  const auto logarithmic=zero_detail::logarithmic_fit(x,maxit);
  auto profile=[&](double r) {
    if(r==0)return ZtnbinomResult{0,1-logarithmic.theta,logarithmic.logLik,0,ZtnbinomStatus::Ok};
    const double p=zero_detail::nb_prob(r,m,maxit);
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

#include "ztnbinom_certificate.hpp"

#include "ztnbinom_accuracy.hpp"
namespace cppunivariate {
inline ZtnbinomResult ztnbinom_mle(ispan x,double tol=1e-10,int maxit=200,double size=NAN,double prob=NAN) {
  auto r=ztnbinom_accuracy_search(x,tol,maxit,size,prob);
  if(r.status!=ZtnbinomStatus::Ok)return r;
  double pi=0;
  if(!accuracy_detail::nb_polish(x,r.size,r.prob,pi,std::isnan(size),std::isnan(prob),false,true,tol,maxit,r.stop_detail)) {
    r.status=ZtnbinomStatus::IterationFailure;record_accuracy(r,tol,{r.size,r.prob},{std::max(1.,std::abs(r.size)),std::max(1.,std::abs(r.prob))});return r;
  }
  if(r.size>0&&r.prob<1){r.logLik=0;for(int v:x)r.logLik+=zero_detail::nb_logpmf(v,r.size,r.prob)-std::log(-std::expm1(r.size*std::log(r.prob)));}
  return r;
}
}

namespace cppunivariate {
inline FitOutcome outcome(const ZtnbinomResult& r) {
  if(r.stop_detail.accuracy_stop==AccuracyStop::Tolerance)return FitOutcome::failure(OutcomeReason::Tolerance);
  if(r.stop_detail.accuracy_stop==AccuracyStop::Conditioning)return FitOutcome::failure(OutcomeReason::Conditioning);
  if(r.status==ZtnbinomStatus::IterationFailure && r.budget_exhausted)return FitOutcome::budget();
  switch(r.status) {
    case ZtnbinomStatus::Ok: return FitOutcome::estimate(true);
    case ZtnbinomStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case ZtnbinomStatus::InvalidSize: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case ZtnbinomStatus::PoissonLimit: return FitOutcome::limit(true);
    case ZtnbinomStatus::IterationFailure: return FitOutcome::failure(r.certificate_failure==ZtnbinomCertificateFailure::Arithmetic ? OutcomeReason::Evaluation : OutcomeReason::NoConvergence);
  }
  return {};
}
} // namespace cppunivariate
