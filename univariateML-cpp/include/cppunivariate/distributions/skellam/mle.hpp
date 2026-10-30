#pragma once
#include "../../core/precision.hpp"
#include "dpqr.hpp"
#include "../../core/span.hpp"
#include <vector>
namespace cppunivariate {
enum class SkellamStatus { Ok, InvalidInput, Impossible, IterationFailure };
struct SkellamResult { double mu1,mu2,logLik; int iterations; SkellamStatus status; };
inline SkellamResult skellam_mle(span x,double tol=1e-9,int maxit=150,
    double mu1=special::kNaN,double mu2=special::kNaN) {
  const bool f1=!std::isnan(mu1),f2=!std::isnan(mu2);
  const auto fail=[](SkellamStatus s) {return SkellamResult{special::kNaN,special::kNaN,special::kNaN,0,s};};
  if(x.size()==0 || !(tol>0) || !std::isfinite(tol) || maxit<1 ||
     (f1 && !(std::isfinite(mu1) && mu1>=0)) || (f2 && !(std::isfinite(mu2) && mu2>=0))) return fail(SkellamStatus::InvalidInput);
  CPPUNIV_EXTENDED_REAL sum=0;double xmin=INFINITY,xmax=-INFINITY;
  for(double v:x) {if(!std::isfinite(v) || v!=std::floor(v)) return fail(SkellamStatus::InvalidInput);sum+=v;xmin=std::min(xmin,v);xmax=std::max(xmax,v);}
  const double m=static_cast<double>(sum/x.size());
  const auto ll=[&](double a,double b) {double out=0;for(double v:x) out+=skellam_detail::logmass(v,a,b);return out;};
  if(f1 && f2) {const double l=ll(mu1,mu2);if(!std::isfinite(l)) return fail(SkellamStatus::Impossible);return {mu1,mu2,l,0,SkellamStatus::Ok};}
  if((f1 && mu1==0) || (f2 && mu2==0)) {
    if((f1 && xmax>0) || (f2 && xmin<0)) return fail(SkellamStatus::Impossible);
    const double a=f1?0:m,b=f2?0:-m;
    return {a,b,ll(a,b),0,SkellamStatus::Ok};
  }
  if(!f1 && !f2 && xmin==xmax) {
    const double a=std::max(m,0.0),b=std::max(-m,0.0);
    return {a,b,ll(a,b),0,SkellamStatus::Ok};
  }
  if(f1 || f2) {
    const auto value=[&](double t) {return f1 ? ll(mu1,t) : ll(t,mu2);};
    const auto score=[&](double t) {
      const double a=f1?mu1:t,b=f2?mu2:t,shift=f1?1.0:-1.0;
      double out=0;
      for(double v:x) out+=std::expm1(skellam_detail::logmass(v+shift,a,b)-skellam_detail::logmass(v,a,b));
      return out;
    };
    const double face=value(0);
    // Conditional J has (j+1)*p(j+1)/p(j)=a*b/(j+|k|+1), decreasing.
    // Hence Var(J)<E(J) and d2 log p(k)/d a2=(Var(Y)-E(Y))/a2<0.
    // The likelihood is strictly concave in either single free rate.
    double face_score=0;
    if(std::isfinite(face)) {
      for(double v:x) face_score+=(f1 ? mu1/(v+1) : mu2/(1-v))-1;
      if(face_score<=0) return {f1?mu1:0,f2?mu2:0,face,0,SkellamStatus::Ok};
    }
    double lo=0,hi=std::max(1.0,(f1?mu1:mu2)+std::abs(m)+1);
    int iterations=0;
    while(score(hi)>0 && iterations<maxit) {hi*=2;++iterations;}
    if(!std::isfinite(hi) || iterations==maxit) return fail(SkellamStatus::IterationFailure);
    while(hi-lo>std::min(tol,1e-9)*(1+lo) && iterations<maxit) {
      const double mid=lo+(hi-lo)/2;
      const double g=score(mid);
      if(std::isnan(g)) return fail(SkellamStatus::IterationFailure);
      if(g>0) lo=mid;else hi=mid;
      ++iterations;
    }
    if(iterations==maxit) return fail(SkellamStatus::IterationFailure);
    const double t=lo+(hi-lo)/2,l=value(t);
    if(!std::isfinite(l)) return fail(SkellamStatus::IterationFailure);
    if(std::isfinite(face) && l<=face+1e-8*x.size())
      return {f1?mu1:0,f2?mu2:0,face,iterations,SkellamStatus::Ok};
    return {f1?mu1:t,f2?mu2:t,l,iterations,SkellamStatus::Ok};
  }
  // The theta score at fixed product gives mu1-mu2=sample mean.
  // In the joint fit, t is the smaller rate.
  const auto rates=[&](double t) {return std::vector<double>{f1?mu1:(f2?t:t+std::max(m,0.0)),f2?mu2:(f1?t:t+std::max(-m,0.0))};};
  const auto value=[&](double t) {const auto r=rates(t);return ll(r[0],r[1]);};
  double best=value(0),best_t=0;
  // Search all nominated local maxima on a logarithmic grid, comparing the
  // attained zero-rate face exactly. No uniqueness assumption is used.
  constexpr double upper=709;
  std::vector<double> grid,values;
  bool certified=false;
  for(double u=-32;u<=upper;u+=0.5) {
    const double t=std::exp(u),v=value(t);
    grid.push_back(u);values.push_back(v);
    // A convolution mass cannot exceed the largest mass of its Poisson
    // factor of rate >= t. Stirling bounds that mode by
    // 1/sqrt(2*pi*floor(t)). This certifies exclusion of the far tail.
    const double candidate=std::max(best,*std::max_element(values.begin(),values.end()));
    if(t>=2 && -0.5*x.size()*(1.8378770664093454836+std::log(std::floor(t)))<candidate-1e-8*x.size()) {certified=true;break;}
  }
  if(!certified) return fail(SkellamStatus::IterationFailure);
  int iterations=0;
  for(std::size_t i=1;i+1<grid.size();++i) {
    if(!(values[i]>=values[i-1] && values[i]>=values[i+1])) continue;
    double lo=grid[i-1],hi=grid[i+1];
    constexpr double c=0.6180339887498948482;
    double a=hi-c*(hi-lo),b=lo+c*(hi-lo),fa=value(std::exp(a)),fb=value(std::exp(b));
    int it=0;
    for(;it<maxit && hi-lo>std::min(tol,1e-8);++it) {
      if(fa>fb) {hi=b;b=a;fb=fa;a=hi-c*(hi-lo);fa=value(std::exp(a));}
      else {lo=a;a=b;fa=fb;b=lo+c*(hi-lo);fb=value(std::exp(b));}
    }
    iterations=std::max(iterations,it);
    if(it==maxit) return fail(SkellamStatus::IterationFailure);
    const double t=std::exp(fa>fb?a:b),l=std::max(fa,fb);
    if(l>best+1e-8*x.size()) {best=l;best_t=t;}
  }
  if(!std::isfinite(best)) return fail(SkellamStatus::Impossible);
  const auto r=rates(best_t);
  return {r[0],r[1],best,iterations,SkellamStatus::Ok};
}
}
