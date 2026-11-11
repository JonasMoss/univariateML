// Closed-form right censoring; unsupported hold sets are explicit.
#pragma once
#include "../exp/censored_transform.hpp"
namespace cppunivariate {
enum class ParetoCensoredStatus { Ok, InvalidInput, AllCensored, Unbounded, NotImplemented, NumericalFailure };
struct ParetoCensoredResult {
  double shape, min, logLik;
  ParetoCensoredStatus status=ParetoCensoredStatus::Ok;
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
  return {r.coefficient,min,r.logLik,status};
}
} // namespace cppunivariate
