#pragma once
#include "../../core/outcome.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"
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
    for(;j<2000000;++j) {
      if((direction<0 && k<=lo)||(direction>0 && k>=hi)) break;
      const double ratio=direction>0 ? (size<0 ? std::exp(t)/(k+1) : std::exp(t)*(size-k)/(k+1))
        : (size<0 ? k*std::exp(-t) : k*std::exp(-t)/(size-k+1));
      w*=ratio; k+=direction;
      total+=w; offset+=(k-mode)*w;
      if(ratio<1 && w*ratio/(1-ratio)<1e-17*total) break;
    }
    if(j==2000000) return {0,0,false};
  }
  const double logz=kernel(mode,t,size)+std::log(total);
  return {logz,mode+offset/total,std::isfinite(logz)};
}
enum class Status { Ok, InvalidInput, LowerPointMass, UpperPointMass, Unidentified, NumericalFailure };
struct Result { double parameter, logLik; int iterations; Status status; StopDetail stop_detail{}; };
inline Result fit(ispan x,double a,double b,double size) {
  Result r{0,0,0,Status::InvalidInput};
  if(x.size()==0 || std::isnan(a)||std::isnan(b)||a>=b || a==INFINITY || b==-INFINITY) return r;
  if(size>=0 && (!std::isfinite(size)||size!=std::floor(size))) return r;
  const double lo=std::max(0.0,std::floor(a)+1), hi=std::min(b==INFINITY ? b : std::floor(b),size<0 ? INFINITY : size);
  if(lo>hi || !std::isfinite(lo)) return r;
  double mean=0; for(int v:x) {if(v<lo||v>hi)return r; mean+=double(v);}
  mean/=x.size();
  if(lo==hi) {r.status=Status::Unidentified; return r;}
  if(mean==lo) {if(lo==0){r.parameter=0;r.status=Status::Ok;}else r.status=Status::LowerPointMass;
    if(r.status==Status::LowerPointMass) r.stop_detail=discrete_point(lo);
    return r;
  }
  if(mean==hi) {if(size>=0 && hi==size){r.parameter=1;r.status=Status::Ok;}else r.status=Status::UpperPointMass;
    if(r.status==Status::UpperPointMass) r.stop_detail=discrete_point(hi);
    return r;
  }
  const double centre=size<0 ? std::log(mean) : std::log(mean/(size-mean));
  double width=1, left=centre-width,right=centre+width;
  for(int j=0;j<12;++j) {
    auto l=mass(left,lo,hi,size), h=mass(right,lo,hi,size);
    if(!l.ok||!h.ok) {r.status=Status::NumericalFailure;return r;}
    if(l.mean<mean && h.mean>mean) break;
    width*=2;
    if(l.mean>=mean)left=centre-width;
    if(h.mean<=mean)right=centre+width;
  }
  for(int j=0;j<120;++j) {
    const double t=(left+right)/2; auto z=mass(t,lo,hi,size);
    if(!z.ok){r.status=Status::NumericalFailure;return r;}
    if(z.mean<mean)left=t;else right=t;
  }
  const double t=(left+right)/2; auto z=mass(t,lo,hi,size);
  for(int v:x) r.logLik+=kernel(v,t,size)-z.logz;
  r.parameter=size<0?std::exp(t):1/(1+std::exp(-t));
  r.iterations=120;r.status=Status::Ok;return r;
}
}
}

namespace cppunivariate {
inline FitOutcome outcome(const truncated_detail::Result& r) {
  switch(r.status) {
    case truncated_detail::Status::Ok: return FitOutcome::estimate(true);
    case truncated_detail::Status::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case truncated_detail::Status::LowerPointMass: return FitOutcome::limit(true);
    case truncated_detail::Status::UpperPointMass: return FitOutcome::limit(true);
    case truncated_detail::Status::Unidentified: return FitOutcome::estimate(true, false);
    case truncated_detail::Status::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
} // namespace cppunivariate
