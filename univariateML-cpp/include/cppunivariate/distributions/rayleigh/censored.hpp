// Closed-form right censoring; unsupported hold sets are explicit.
#pragma once
#include "../exp/censored_transform.hpp"
namespace cppunivariate {
enum class RayleighCensoredStatus { Ok, InvalidInput, AllCensored, Unbounded, NotImplemented, NumericalFailure };
struct RayleighCensoredResult {
  double sigma, logLik;
  RayleighCensoredStatus status=RayleighCensoredStatus::Ok;
};
inline RayleighCensoredResult rayleigh_censored_mle(CensoredData x, double sigma=NAN) {
  if(!std::isnan(sigma))return {NAN,NAN,RayleighCensoredStatus::NotImplemented};
  const auto r=censored_detail::fit(x,censored_detail::Transform::Rayleigh,2);
  const auto status=r.status==ExpCensoredStatus::Ok?RayleighCensoredStatus::Ok:
    r.status==ExpCensoredStatus::InvalidInput?RayleighCensoredStatus::InvalidInput:
    r.status==ExpCensoredStatus::AllCensored?RayleighCensoredStatus::AllCensored:
    r.status==ExpCensoredStatus::Unbounded?RayleighCensoredStatus::Unbounded:
    r.status==ExpCensoredStatus::NotImplemented?RayleighCensoredStatus::NotImplemented:
    RayleighCensoredStatus::NumericalFailure;
  return {r.coefficient,r.logLik,status};
}
} // namespace cppunivariate
