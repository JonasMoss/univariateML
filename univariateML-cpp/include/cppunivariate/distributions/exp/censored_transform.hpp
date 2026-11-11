#pragma once
#include "censored.hpp"
namespace cppunivariate {
namespace censored_detail {
enum class Transform { Weibull, Rayleigh, Pareto, Lomax };
struct Result { double coefficient, logLik; ExpCensoredStatus status; };
inline Result fit(CensoredData x, Transform kind, double held) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto c=classify_censored(x,kind==Transform::Pareto?held:0);
  if(c.status!=CensoredStatus::Ok || x.lower.empty() || !(held>0 && std::isfinite(held)))
    return {nan,nan,ExpCensoredStatus::InvalidInput};
  if(c.left || c.interval)return {nan,nan,ExpCensoredStatus::NotImplemented};
  double big=0;
  std::size_t positive=0;
  for(std::size_t i=0;i<x.lower.size();++i) {
    const double v=x.lower[i];
    if(!(v>=0 && std::isfinite(v)) || (kind==Transform::Pareto && v<held) ||
       (v==0 && x.lower[i]==x.upper[i] && kind!=Transform::Lomax))
      return {nan,nan,ExpCensoredStatus::InvalidInput};
    big=std::max(big,v);
    if(v>0)++positive;
  }
  if(!c.exact)return {nan,nan,ExpCensoredStatus::AllCensored};
  const double d=static_cast<double>(c.exact);
  CPPUNIV_EXTENDED_REAL total=0,jac=0,shifted=0;
  const double power=kind==Transform::Rayleigh?2:held;
  for(std::size_t i=0;i<x.lower.size();++i) {
    const double v=x.lower[i];
    const bool exact=v==x.upper[i];
    if(kind==Transform::Weibull || kind==Transform::Rayleigh) {
      // Normalize before powering; never form a dimensional square/power.
      const CPPUNIV_EXTENDED_REAL ratio=big>0?static_cast<CPPUNIV_EXTENDED_REAL>(v)/big:0;
      const CPPUNIV_EXTENDED_REAL logratio=ratio>0?std::log(ratio):std::log(v)-std::log(big);
      total+=v==0?0:ratio>0?std::pow(ratio,power):std::exp(power*logratio);
      if(kind==Transform::Weibull && power<.01 && v>0)
        shifted+=std::expm1(power*logratio);
    } else if(kind==Transform::Pareto) {
      const CPPUNIV_EXTENDED_REAL relative=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-held)/held;
      total+=std::isfinite(relative)?std::log1p(relative):std::log(v)-std::log(held);
      if(exact)jac-=std::log(v);
    } else {
      // log1p retains small products; log-domain fallback handles overflow.
      const CPPUNIV_EXTENDED_REAL product=static_cast<CPPUNIV_EXTENDED_REAL>(held)*v;
      const CPPUNIV_EXTENDED_REAL t=std::isfinite(product)?std::log1p(product):std::log(held)+std::log(v);
      total+=t;
      if(exact)jac+=std::log(held)-t;
    }
  }
  if(total==0)return {nan,nan,kind==Transform::Lomax && big>0?
      ExpCensoredStatus::NumericalFailure:ExpCensoredStatus::Unbounded};
  double coefficient, ll;
  if(kind==Transform::Weibull || kind==Transform::Rayleigh) {
    CPPUNIV_EXTENDED_REAL logfactor=std::log(total/(kind==Transform::Rayleigh?2*d:d))/power;
    CPPUNIV_EXTENDED_REAL factor=std::pow(total/(kind==Transform::Rayleigh?2*d:d),1/static_cast<CPPUNIV_EXTENDED_REAL>(power));
    if(kind==Transform::Weibull && power<.01) {
      const CPPUNIV_EXTENDED_REAL n=positive;
      logfactor=(std::log1p((n-d)/d)+std::log1p(shifted/n))/power;
      factor=std::exp(logfactor);
    }
    coefficient=factor>std::numeric_limits<double>::min() && factor<std::numeric_limits<double>::max()?
        big*static_cast<double>(factor):static_cast<double>(std::exp(std::log(big)+logfactor));
    if(!(coefficient>0 && std::isfinite(coefficient)))
      return {coefficient,nan,ExpCensoredStatus::NumericalFailure};
    // Evaluate using ratios to the returned scale, avoiding log cancellation.
    CPPUNIV_EXTENDED_REAL value=0;
    for(std::size_t i=0;i<x.lower.size();++i) {
      const double v=x.lower[i];
      const CPPUNIV_EXTENDED_REAL z=static_cast<CPPUNIV_EXTENDED_REAL>(v)/coefficient;
      if(kind==Transform::Rayleigh) {
        value-=z*z/2;
        if(v==x.upper[i])value+=std::log(v)-2*std::log(coefficient);
      } else {
        const CPPUNIV_EXTENDED_REAL logratio=z>0 && std::isfinite(z)?
            std::log(z):std::log(v)-std::log(coefficient);
        value-=std::exp(power*logratio);
        if(v==x.upper[i])value+=std::log(power)-std::log(coefficient)+(power-1)*logratio;
      }
    }
    ll=static_cast<double>(value);
  } else {
    coefficient=static_cast<double>(d/total);
    ll=static_cast<double>(d*std::log(coefficient)-coefficient*total+jac);
  }
  if(!(coefficient>0 && std::isfinite(coefficient) && std::isfinite(ll)))
    return {coefficient,ll,ExpCensoredStatus::NumericalFailure};
  return {coefficient,ll,ExpCensoredStatus::Ok};
}
} // namespace censored_detail
} // namespace cppunivariate
