#pragma once
#include "../geom/truncation.hpp"
#include "tpois_dpqr.hpp"
namespace cppunivariate {
// A held parent conditional law requires positive conditioning probability.
// The positive-truncation extension is fitted separately by ztpois_mle.
inline HeldWindowResult held_pois_window_mle(ispan x,double lambda,double a,double b) {
  if(x.empty())return {NAN,HeldWindowStatus::Empty};
  if(!(lambda>=0 && std::isfinite(lambda)))return {};
  for(int v:x)if(v<0)return {};
  if(std::isnan(a))a=*std::min_element(x.begin(),x.end());
  if(std::isnan(b))b=*std::max_element(x.begin(),x.end());
  const double lo=std::max(0.,std::ceil(a)),hi=std::floor(b);
  if(!(lo<=hi) || !std::isfinite(lo))return {};
  for(int v:x)if(v<lo || v>hi)return {};
  if(lambda==0) {
    if(lo>0 || hi<0 || !std::all_of(x.begin(),x.end(),[](int v){return v==0;}))return {};
    return {0,HeldWindowStatus::Ok};
  }
  // Every compatible count has strictly positive conditional mass.
  return held_window_likelihood(x,[&](double v){return distributions::dtpois(v,lambda,std::max(-1.,std::ceil(a)-1),std::floor(b),true);},true);
}
}
