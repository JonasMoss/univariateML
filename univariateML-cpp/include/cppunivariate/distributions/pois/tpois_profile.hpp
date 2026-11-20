#pragma once
#include "../../core/outcome.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/span.hpp"
#include "../../core/work_budget.hpp"
#include "../../special/special.hpp"
#include "truncation.hpp"
#include "../binom/truncation.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace truncated_detail {
// Sum relative to the conditional mode. Recurrences avoid subtracting CDFs.
struct Mass { double logz, mean; bool ok; };
inline double kernel(double k, double t, double size) {
  return k*t-special::lgamma(k+1) - (size<0 ? 0 : special::lgamma(size-k+1));
}
inline Mass mass(double t, double lo, double hi, double size) {
  double mode=size<0 ? std::exp(t) : (size+1)/(1+std::exp(-t));
  mode=std::floor(std::max(lo,std::min(hi,mode)));
  if(!std::isfinite(mode)) return {0,0,false};
  double total=1, offset=0;
  for(int direction : {-1,1}) {
    double k=mode, w=1;
    int j=0;
    const int cap=2000000; // Fully held likelihood evaluator.
    for(;j<cap;++j) {
      if((direction<0 && k<=lo)||(direction>0 && k>=hi)) break;
      const double ratio=direction>0 ? (size<0 ? std::exp(t)/(k+1) : std::exp(t)*(size-k)/(k+1))
        : (size<0 ? k*std::exp(-t) : k*std::exp(-t)/(size-k+1));
      w*=ratio; k+=direction;
      total+=w; offset+=(k-mode)*w;
      if(ratio<1 && w*ratio/(1-ratio)<1e-17*total) break;
    }
    if(j==cap) return {0,0,false};
  }
  const double logz=kernel(mode,t,size)+std::log(total);
  return {logz,mode+offset/total,std::isfinite(logz)};
}
enum class Status { Ok, InvalidInput, LowerPointMass, UpperPointMass, Unidentified, NumericalFailure };
struct Result { double parameter, logLik; int iterations; Status status; StopDetail stop_detail{}; bool unique=true; };
inline Result fit(ispan x,double a,double b,double size,double tol=1e-10,int maxit=200) {
  Result r{0,0,0,Status::InvalidInput};
  if(x.size()==0 || std::isnan(a)||std::isnan(b)||a>=b || a==INFINITY || b==-INFINITY) return r;
  if(size>=0 && (!std::isfinite(size)||size!=std::floor(size))) return r;
  const double lo=std::max(0.0,std::floor(a)+1), hi=std::min(b==INFINITY ? b : std::floor(b),size<0 ? INFINITY : size);
  if(lo>hi || !std::isfinite(lo)) return r;
  for(int v:x)if(v<lo || v>hi)return r;
  if(lo==hi)return {0,0,0,Status::Unidentified};
  const auto fit=size<0?truncated<PoisTruncationTraits>::fit(x,lo,hi,{},NAN,tol,maxit):
    truncated<BinomTruncationTraits>::fit(x,lo,hi,{size},NAN,tol,maxit);
  r.parameter=fit.parameter;r.logLik=fit.logLik;r.iterations=fit.iterations;
  switch(fit.status) {
    case TruncationStatus::Ok: r.status=Status::Ok;break;
    case TruncationStatus::LowerLimit:
      r.status=Status::LowerPointMass;r.stop_detail=discrete_point(lo);break;
    case TruncationStatus::UpperLimit:
      r.status=Status::UpperPointMass;r.stop_detail=discrete_point(hi);break;
    case TruncationStatus::NumericalFailure: r.status=Status::NumericalFailure;break;
    default: r.status=Status::InvalidInput;break;
  }
  return r;
}
}
}

namespace cppunivariate {
inline bool family_point(const truncated_detail::Result&, const FitCandidate& c) {
  return c.model == StopModel::None && !c.coef.empty() && std::isfinite(c.coef[0]) && c.coef[0] > 0;
}
// Singleton-support non-identification lists no maximizer.
inline bool accurate_point(const truncated_detail::Result&, const FitCandidate&) { return false; }
inline FitOutcome outcome(const truncated_detail::Result& r) {
  switch(r.status) {
    case truncated_detail::Status::Ok: return FitOutcome::estimate(true,r.unique);
    case truncated_detail::Status::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case truncated_detail::Status::LowerPointMass: return FitOutcome::limit(true);
    case truncated_detail::Status::UpperPointMass: return FitOutcome::limit(true);
    case truncated_detail::Status::Unidentified: return nonunique_outcome_of(r, true);
    case truncated_detail::Status::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
} // namespace cppunivariate
