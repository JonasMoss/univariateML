// Right-censored exponential likelihood; no changes to the exact-data API.
#pragma once
#include "../../core/censored.hpp"
#include "../../core/precision.hpp"
#include "mle.hpp"
namespace cppunivariate {
enum class ExpCensoredStatus { Ok, InvalidInput, AllCensored, Unbounded, NotImplemented, NumericalFailure };
struct ExpCensoredResult {
  double rate, logLik;
  ExpCensoredStatus status = ExpCensoredStatus::Ok;
  bool unique=true;
};
// No exact event: the positive rate has an unattained zero-rate supremum.
// Collapse with exact events instead has an unbounded density likelihood.
template<class Status> inline FitOutcome censored_outcome(Status status) {
  switch(status) {
    case Status::Ok: return FitOutcome::singleton(true);
    case Status::AllCensored: return FitOutcome::nonexistence(OutcomeReason::Unattained);
    case Status::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case Status::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case Status::NotImplemented: return FitOutcome::failure(OutcomeReason::Unsupported);
    case Status::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
inline FitOutcome outcome(const ExpCensoredResult& r) { return r.status==ExpCensoredStatus::Ok?FitOutcome::estimate(true,r.unique):censored_outcome(r.status); }
inline ExpCensoredResult exp_censored_mle(CensoredData x,
    double rate=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto c=classify_censored(x,0);
  if(c.status!=CensoredStatus::Ok || x.lower.empty() ||
     (!std::isnan(rate) && !(rate>0 && std::isfinite(rate))))
    return {nan,nan,ExpCensoredStatus::InvalidInput};
  if(c.left || c.interval)return {nan,nan,ExpCensoredStatus::NotImplemented};
  double big=0;
  for(std::size_t i=0;i<x.lower.size();++i) {
    const double v=x.lower[i];
    if(!std::isfinite(v) || (v<0 && v==x.upper[i]))return {nan,nan,ExpCensoredStatus::InvalidInput};
    big=std::max(big,v);
  }
  CPPUNIV_EXTENDED_REAL scaled=0;
  if(big>0)for(double v:x.lower)scaled+=std::max(0.,v)/big;
  const double d=static_cast<double>(c.exact);
  if(std::isnan(rate)) {
    if(!c.exact)return big==0?ExpCensoredResult{1,0,ExpCensoredStatus::Ok,false}:ExpCensoredResult{nan,nan,ExpCensoredStatus::AllCensored};
    if(big==0)return {nan,nan,ExpCensoredStatus::Unbounded};
    // Match the exact API's ordinary-data rounding; scale when necessary.
    double total=0;for(double v:x.lower)total+=std::max(0.,v);
    rate=std::isfinite(total)?d/total:static_cast<double>((d/scaled)/big);
    const double ll=d*(std::log(rate)-1);
    if(!(rate>0 && std::isfinite(rate) && std::isfinite(ll)))
      return {rate,ll,ExpCensoredStatus::NumericalFailure};
    return {rate,ll};
  }
  const double ll=static_cast<double>(d*std::log(rate)-
      (static_cast<CPPUNIV_EXTENDED_REAL>(rate)*big)*scaled);
  return {rate,ll,std::isfinite(ll)?ExpCensoredStatus::Ok:ExpCensoredStatus::NumericalFailure};
}
} // namespace cppunivariate
