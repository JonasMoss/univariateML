// Closed-form right censoring; unsupported hold sets are explicit.
#pragma once
#include "../exp/censored_transform.hpp"
#include "mle.hpp"
namespace cppunivariate {
enum class LomaxCensoredStatus { Ok, InvalidInput, AllCensored, Unbounded, NotImplemented, NumericalFailure };
struct LomaxCensoredResult {
  double lambda, kappa, logLik;
  LomaxCensoredStatus status=LomaxCensoredStatus::Ok;
};
inline LomaxCensoredResult lomax_censored_mle(CensoredData x, double lambda=NAN, double kappa=NAN) {
  if(std::isnan(lambda) || !std::isnan(kappa))return {NAN,NAN,NAN,LomaxCensoredStatus::NotImplemented};
  const auto r=censored_detail::fit(x,censored_detail::Transform::Lomax,lambda);
  // Preserve exact-data rounding when its result agrees with the stable
  // closed form well inside the 1e-10 coefficient accuracy contract.
  if(r.status==ExpCensoredStatus::Ok && classify_censored(x).exact==x.lower.size()) {
    const auto exact=lomax_mle(x.lower,1e-10,1000,NAN,lambda,NAN);
    if(exact.status==LomaxStatus::Ok && std::abs(exact.kappa/r.coefficient-1)<1e-12)
      return {lambda,exact.kappa,exact.logLik};
  }
  const auto status=r.status==ExpCensoredStatus::Ok?LomaxCensoredStatus::Ok:
    r.status==ExpCensoredStatus::InvalidInput?LomaxCensoredStatus::InvalidInput:
    r.status==ExpCensoredStatus::AllCensored?LomaxCensoredStatus::AllCensored:
    r.status==ExpCensoredStatus::Unbounded?LomaxCensoredStatus::Unbounded:
    r.status==ExpCensoredStatus::NotImplemented?LomaxCensoredStatus::NotImplemented:
    LomaxCensoredStatus::NumericalFailure;
  return {lambda,r.coefficient,r.logLik,status};
}
} // namespace cppunivariate
