// Traits-based Bernoulli hurdle. The positive likelihood and its status remain
// family-specific; no kind switch is needed in a new modifier implementation.
#pragma once
#include "span.hpp"
#include <cmath>
#include <utility>
#include <vector>
namespace cppunivariate {
enum class HurdleStatus { Ok, InvalidInput, AllZero, PositiveFitFailure };
template<class Positive> struct HurdleResult {
  Positive positive{};
  double p0=NAN,logLik=NAN;
  HurdleStatus status=HurdleStatus::InvalidInput;
};
template<class Traits> struct zero_modified {
  // Remaining arguments are exactly the traits' positive-fitter holds and
  // controls. NaN p0 estimates the atom; a number holds it.
  template<class... Args> static auto fit(ispan x,double p0,Args... args) {
    HurdleResult<decltype(Traits::truncated(x,args...))> out;
    if(x.empty() || (!std::isnan(p0) && !(p0>=0 && p0<=1)))return out;
    std::vector<int> positives;
    for(int v:x){if(v<0)return out;if(v>0)positives.push_back(v);}
    const double n=x.size(),k=positives.size();
    out.p0=std::isnan(p0)?(n-k)/n:p0;
    if((k<n && out.p0==0) || (k>0 && out.p0==1))return out;
    if(positives.empty()) {
      out.logLik=n*std::log(out.p0);out.status=HurdleStatus::AllZero;return out;
    }
    out.positive=Traits::truncated(ispan{positives.data(),positives.size()},args...);
    if(!Traits::positive_ok(out.positive) || !std::isfinite(out.positive.logLik)) {
      out.status=HurdleStatus::PositiveFitFailure;return out;
    }
    out.logLik=out.positive.logLik;
    if(k<n)out.logLik+=(n-k)*std::log(out.p0);
    out.logLik+=k*std::log1p(-out.p0);
    out.status=HurdleStatus::Ok;return out;
  }
};
}
