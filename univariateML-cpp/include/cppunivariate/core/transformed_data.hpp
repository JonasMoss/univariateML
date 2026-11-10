#pragma once
#include "span.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace transformed_detail {
// Two-component arithmetic is used only for the absolute log anchor and
// coefficient restoration. Dispersion always comes from raw differences.
struct Pair { double hi,lo; };
inline Pair add(Pair a,Pair b) {
  const double s=a.hi+b.hi,v=s-a.hi;
  const double e=(a.hi-(s-v))+(b.hi-v)+a.lo+b.lo;
  const double h=s+e;return {h,e-(h-s)};
}
inline Pair neg(Pair a) { return {-a.hi,-a.lo}; }
inline Pair mul(Pair a,Pair b) {
  const double p=a.hi*b.hi,e=std::fma(a.hi,b.hi,-p)+a.hi*b.lo+a.lo*b.hi;
  return add({p,0},{e,a.lo*b.lo});
}
inline Pair div(Pair a,Pair b) {
  const double q=a.hi/b.hi;
  const Pair r=add(a,neg(mul({q,0},b)));
  return add({q,0},{(r.hi+r.lo)/b.hi,0});
}
inline double value(Pair a) { return a.hi+a.lo; }
// Log anchor with a small residual, independent of long-double width.
// frexp reduces |(m-1)/(m+1)| below .172; 32 atanh terms leave < 2e-50.
inline Pair log_pair(double x) {
  int e=0;double m=std::frexp(x,&e);
  if(m<.7071067811865475244){m*=2;--e;}
  const Pair z=div({m-1,0},add({m,0},{1,0})),z2=mul(z,z);
  Pair term=z,sum=z;
  for(int j=1;j<32;++j){term=mul(term,z2);sum=add(sum,div(term,{double(2*j+1),0}));}
  return add(mul({2,0},sum),mul({double(e),0},{.6931471805599453094,2.3190468138462995584e-17}));
}
inline Pair log_complement(double x) {
  const Pair c=add({1,0},{-x,0});
  return add(log_pair(c.hi),{std::log1p(c.lo/c.hi),0});
}
inline double log_ratio(double x,double anchor) {
  const double d=(x-anchor)/anchor;
  return std::abs(d)<.5 ? std::log1p(d) : std::log(x)-std::log(anchor);
}
struct Sum {
  double hi=0,lo=0;
  void push(double x){
    if(!std::isfinite(x) || !std::isfinite(hi)){hi+=x;lo=0;return;}
    const Pair s=add({hi,lo},{x,0});hi=s.hi;lo=s.lo;
  }
  double get() const{return hi+lo;}
};
struct Logs {
  std::vector<double> delta;
  Pair anchor{NAN,0}, mean{0,0};
  double sum=0, jacobian=0;
  bool valid=false;
};
inline Logs logs(span x,bool logit=false) {
  Logs r;if(x.empty())return r;
  for(double v:x)if(!(v>0 && std::isfinite(v) && (!logit || v<1)))return r;
  r.anchor=log_pair(x[0]);
  if(logit)r.anchor=add(r.anchor,neg(log_complement(x[0])));
  Sum total,jac;
  for(double v:x){
    double d=log_ratio(v,x[0]);
    if(logit) {
      // 1-v may round for tiny v: log1p retains that complement.
      d-=std::log1p((x[0]-v)/(1-x[0]));
      jac.push(-std::log(v)-std::log1p(-v));
    } else jac.push(-std::log(v));
    r.delta.push_back(d);total.push(d);
    Pair absolute=log_pair(v);if(logit)absolute=add(absolute,neg(log_complement(v)));
    r.mean=add(r.mean,div(absolute,{double(x.size()),0}));
  }
  r.sum=value(add(mul({double(x.size()),0},r.anchor),{total.get(),0}));
  r.jacobian=jac.get();r.valid=true;return r;
}
inline bool restored(double ideal,double actual,double tol,double magnitude) {
  return std::isfinite(actual) && ideal-actual<=tol*std::max(1.0,std::abs(ideal))+
    128*std::numeric_limits<double>::epsilon()*(1+magnitude);
}
// Closed-form normal sufficient statistics in centered log coordinates.
struct Normal { double mean,sd,ll; int status; bool certified=true; };
inline Normal normal(const Logs& l,double mean,double sd) {
  if(!l.valid || (!std::isnan(mean)&&!std::isfinite(mean)) ||
     (!std::isnan(sd)&&!(sd>0 && std::isfinite(sd))))return {NAN,NAN,NAN,1};
  const double n=double(l.delta.size());Sum sum;
  for(double d:l.delta)sum.push(d/n);
  const double center=sum.get();
  const bool free_mean=std::isnan(mean),free_sd=std::isnan(sd);
  if(free_mean)mean=value(l.mean);
  const bool constant=std::all_of(l.delta.begin(),l.delta.end(),[](double d){return d==0;});
  if(free_sd && constant && free_mean)return {mean,0,INFINITY,2};
  // A zero residual is nonexistence only when the complete anchor agrees.
  // A held binary64 log can differ from the exact log by its low component.
  if(free_sd && constant && !free_mean && mean==l.anchor.hi && l.anchor.lo==0)
    return {mean,0,INFINITY,2};
  const double offset=value(add({mean,0},neg(l.anchor)));
  double radius=0;for(double d:l.delta)radius=std::max(radius,std::abs(d-center));
  double actual_radius=0;for(double d:l.delta)actual_radius=std::max(actual_radius,std::abs(d-offset));
  if(free_sd && actual_radius==0)return {mean,0,INFINITY,2};
  Sum square,ideal_square;
  if(actual_radius>0)for(double d:l.delta){const double z=(d-offset)/actual_radius;square.push(z*z);}
  if(radius>0)for(double d:l.delta){const double z=(d-center)/radius;ideal_square.push(z*z);}
  const double ideal_sd=radius*std::sqrt(ideal_square.get()/n);
  if(free_sd)sd=free_mean?ideal_sd:actual_radius*std::sqrt(square.get()/n);
  if(!(sd>=std::numeric_limits<double>::min() && std::isfinite(sd)))return {mean,sd,NAN,3};
  const double ratio=actual_radius/sd;
  const double penalty=square.get()*ratio*ratio;
  const double ll=-n*(std::log(sd)+.91893853320467274178)-.5*penalty+l.jacobian;
  // Free mean restoration can visibly lower a concentrated likelihood.
  const double ideal=free_sd?-n*(std::log(sd)+.91893853320467274178)-n/2+l.jacobian:
      -n*(std::log(sd)+.91893853320467274178)-.5*ideal_square.get()*(radius/sd)*(radius/sd)+l.jacobian;
  const bool certified=!free_mean || restored(ideal,ll,1e-10,std::abs(ideal)+std::abs(ll));
  return {mean,sd,ll,std::isfinite(ll)?0:3,certified};
}
} // namespace transformed_detail
} // namespace cppunivariate
