#pragma once
#include "../geom/truncation.hpp"
#include "tbinom_dpqr.hpp"
namespace cppunivariate {
// The positive-truncation endpoint extension belongs to ztbinom_mle;
// this kernel evaluates a proper held parent on a positive-probability window.
inline HeldWindowResult held_binom_window_mle(ispan x,double size,double prob,double a,double b) {
  if(x.empty())return {NAN,HeldWindowStatus::Empty};
  if(!(size>=0 && std::isfinite(size) && size==std::floor(size)) ||
     !(prob>=0 && prob<=1 && std::isfinite(prob)))return {};
  for(int v:x)if(v<0)return {};
  if(std::isnan(a))a=*std::min_element(x.begin(),x.end());
  if(std::isnan(b))b=*std::max_element(x.begin(),x.end());
  const double lo=std::max(0.,std::ceil(a)),hi=std::min(size,std::floor(b));
  if(!(lo<=hi) || !std::isfinite(lo))return {};
  for(int v:x)if(v<lo || v>hi)return {};
  if(size==0 || prob==0 || prob==1) {
    const double atom=prob==1?size:0;
    if(atom<lo || atom>hi || !std::all_of(x.begin(),x.end(),[&](int v){return v==atom;}))return {};
    return {0,HeldWindowStatus::Ok};
  }
  // Every compatible count has strictly positive conditional mass.
  return held_window_likelihood(x,[&](double v){return distributions::dtbinom(v,prob,std::max(-1.,std::ceil(a)-1),std::floor(b),size,true);},true);
}
}
