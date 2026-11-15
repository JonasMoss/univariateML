#pragma once
#include "../../core/outcome.hpp"
#include "ztpois_profile.hpp"
#include "ztpois_dpqr.hpp"
namespace cppunivariate {
enum class ZtpoisStatus { Ok, InvalidInput, NumericalFailure };
struct ZtpoisResult { double lambda, logLik; int iterations; ZtpoisStatus status; };
inline ZtpoisResult ztpois_mle(ispan x, double fixed=std::numeric_limits<double>::quiet_NaN()) {
  if(!zero_detail::positive(x)) return {0,0,0,ZtpoisStatus::InvalidInput};
  if(!std::isnan(fixed)) {
    if(!(fixed>=0 && std::isfinite(fixed)))return {0,0,0,ZtpoisStatus::InvalidInput};
    double ll=0;for(int v:x)ll+=distributions::dztpois(v,fixed,true);
    if(!std::isfinite(ll))return {fixed,ll,0,fixed==0?ZtpoisStatus::InvalidInput:ZtpoisStatus::NumericalFailure};
    return {fixed,ll,0,ZtpoisStatus::Ok};
  }
  const double m=zero_detail::mean(x);
  if(m==1) return {0,0,0,ZtpoisStatus::Ok};
  double lo=0, hi=m;
  for(int i=0;i<100;++i) {
    const double l=(lo+hi)/2;
    if(l/(-std::expm1(-l))<m) lo=l; else hi=l;
  }
  const double l=(lo+hi)/2;
  double ll=0;
  for(int v:x) ll+=distributions::dztpois(v,l,true);
  return {l,ll,100,std::isfinite(ll)?ZtpoisStatus::Ok:ZtpoisStatus::NumericalFailure};
}
}

namespace cppunivariate {
inline FitOutcome outcome(const ZtpoisResult& r) {
  switch(r.status) {
    case ZtpoisStatus::Ok: return FitOutcome::estimate(true);
    case ZtpoisStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case ZtpoisStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
} // namespace cppunivariate
