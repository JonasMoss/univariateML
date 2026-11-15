#pragma once
#include "../../core/truncation.hpp"
#include "tnorm_mle.hpp"
#include "tnorm_dpqr.hpp"
#include "../geom/truncation.hpp"
namespace cppunivariate {
struct NormTruncationTraits {};
// Joint concavity in (mean/sd^2, -1/(2 sd^2)); the existing solver also
// compares the exponential/uniform face and supports every coefficient hold.
template<> struct truncated<NormTruncationTraits> {
  static TnormResult fit(span x,double a,double b,double mean=NAN,double sd=NAN,double tol=1e-10,int maxit=200) {
    return tnorm_mle(x,a,b,tol,maxit,mean,sd);
  }
};
}

namespace cppunivariate {
// Fully held parent coefficients still leave estimated truncation endpoints.
// Keep the existing public-density evaluation after the core classifies that
// likelihood design, rather than bypassing the shrinking-window escape.
inline HeldWindowResult held_normal_window_mle(span x,double mean,double sd,double a,double b) {
  if(x.empty())return {NAN,HeldWindowStatus::Empty};
  if(!std::isfinite(mean) || !(sd>0 && std::isfinite(sd)))return {};
  for(double v:x)if(!std::isfinite(v))return {NAN,HeldWindowStatus::Nonfinite};
  if((std::isnan(a) || std::isnan(b)) && (std::isnan(a) || a==x[0]) &&
     (std::isnan(b) || b==x[0]) && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
    return {NAN,HeldWindowStatus::Unbounded};
  if(std::isnan(a))a=*std::min_element(x.begin(),x.end());
  if(std::isnan(b))b=*std::max_element(x.begin(),x.end());
  if(!(a<b) || a==INFINITY || b==-INFINITY)return {};
  for(double v:x)if(v<a || v>b)return {};
  // A normal density is strictly positive throughout a proper window.
  return held_window_likelihood(x,[&](double v){return distributions::dtnorm(v,mean,sd,a,b,true);},true);
}
}
