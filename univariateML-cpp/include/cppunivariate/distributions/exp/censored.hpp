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
};
inline ExpCensoredResult exp_censored_mle(CensoredData x,
    double rate=std::numeric_limits<double>::quiet_NaN()) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto c=classify_censored(x,0);
  if(c.status!=CensoredStatus::Ok || x.lower.empty() ||
     (!std::isnan(rate) && !(rate>0 && std::isfinite(rate))))
    return {nan,nan,ExpCensoredStatus::InvalidInput};
  if(c.left || c.interval)return {nan,nan,ExpCensoredStatus::NotImplemented};
  double big=0;
  for(double v:x.lower) {
    if(!(v>=0 && std::isfinite(v)))return {nan,nan,ExpCensoredStatus::InvalidInput};
    big=std::max(big,v);
  }
  CPPUNIV_EXTENDED_REAL scaled=0;
  if(big>0)for(double v:x.lower)scaled+=v/big;
  const double d=static_cast<double>(c.exact);
  if(std::isnan(rate)) {
    if(!c.exact)return {nan,nan,ExpCensoredStatus::AllCensored};
    if(big==0)return {nan,nan,ExpCensoredStatus::Unbounded};
    // Match the exact API's ordinary-data rounding; scale when necessary.
    double total=0;for(double v:x.lower)total+=v;
    rate=std::isfinite(total)?d/total:static_cast<double>((d/scaled)/big);
    const double ll=d*(std::log(rate)-1);
    if(!(rate>0 && std::isfinite(rate) && std::isfinite(ll)))
      return {rate,ll,ExpCensoredStatus::NumericalFailure};
    return {rate,ll};
  }
  const double ll=static_cast<double>(d*std::log(rate)-
      (static_cast<CPPUNIV_EXTENDED_REAL>(rate)*big)*scaled);
  // A fixed model may have log likelihood -infinity from range overflow.
  return {rate,ll};
}
} // namespace cppunivariate
