// Closed-form right censoring; unsupported hold sets are explicit.
#pragma once
#include "../exp/censored_transform.hpp"
namespace cppunivariate {
enum class ParetoCensoredStatus { Ok, InvalidInput, AllCensored, Unbounded, NotImplemented, NumericalFailure };
struct ParetoCensoredResult {
  double shape, min, logLik;
  ParetoCensoredStatus status=ParetoCensoredStatus::Ok;
  bool unique=true;
  // Preserve the legacy raw validation code while recording the proved collapse.
  bool unbounded_after_support_clamp=false;
};
inline ParetoCensoredResult pareto_censored_mle(CensoredData x, double shape=NAN, double min=NAN) {
  if(!std::isnan(shape) || std::isnan(min))return {NAN,NAN,NAN,ParetoCensoredStatus::NotImplemented};
  const auto r=censored_detail::fit(x,censored_detail::Transform::Pareto,min);
  const auto status=r.status==ExpCensoredStatus::Ok?ParetoCensoredStatus::Ok:
    r.status==ExpCensoredStatus::InvalidInput?ParetoCensoredStatus::InvalidInput:
    r.status==ExpCensoredStatus::AllCensored?ParetoCensoredStatus::AllCensored:
    r.status==ExpCensoredStatus::Unbounded?ParetoCensoredStatus::Unbounded:
    r.status==ExpCensoredStatus::NotImplemented?ParetoCensoredStatus::NotImplemented:
    ParetoCensoredStatus::NumericalFailure;
  ParetoCensoredResult out{r.coefficient,min,r.logLik,status,r.unique};
  if(status==ParetoCensoredStatus::Unbounded)for(std::size_t i=0;i<x.lower.size();++i)
    if(x.lower[i]<min && x.lower[i]!=x.upper[i]) {
      out.status=ParetoCensoredStatus::InvalidInput;out.unbounded_after_support_clamp=true;break;
    }
  return out;
}
inline FitOutcome outcome(const ParetoCensoredResult& r) { return r.unbounded_after_support_clamp?FitOutcome::nonexistence(OutcomeReason::Unbounded):r.status==ParetoCensoredStatus::Ok?FitOutcome::estimate(true,r.unique):censored_outcome(r.status); }
} // namespace cppunivariate
