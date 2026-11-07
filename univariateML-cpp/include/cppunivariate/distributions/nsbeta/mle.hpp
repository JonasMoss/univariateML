#pragma once
#include "../../core/work_budget.hpp"
#include "../burr/mle.hpp"
#include "../beta/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct NsbetaResult { double shape1,shape2,min,max,logLik; int iterations; BetaStatus status; };

inline NsbetaResult nsbeta_mle(span x, double min, double max, double tol, int maxit, double a=std::numeric_limits<double>::quiet_NaN(), double b=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min)/(max-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min) && std::isfinite(max) && max>min;
  for(double v:x) valid=valid && std::isfinite(v) && v>min && v<max;
  if(!valid) y.clear();
  if(!std::isnan(a)||!std::isnan(b)){
    const double nan=std::numeric_limits<double>::quiet_NaN();
    for(double v:{a,b})if(!std::isnan(v)&&(!(v>0)||!std::isfinite(v)))valid=false;
    if(!valid||x.size()==0)return {a,b,min,max,nan,0,BetaStatus::IterationFailure};
    double s=0,r=0;for(double v:y){s+=std::log(v);r+=std::log1p(-v);}
    double n=static_cast<double>(x.size());
    if(std::isnan(a))a=std::exp(burr_hold_detail::root([&](double z){double v=std::exp(z);return special::digamma(v+b)-special::digamma(v)+s/n;}));
    if(std::isnan(b))b=std::exp(burr_hold_detail::root([&](double z){double v=std::exp(z);return special::digamma(a+v)-special::digamma(v)+r/n;}));
    double ll=(a-1)*s+(b-1)*r+n*(special::lgamma(a+b)-special::lgamma(a)-special::lgamma(b)-std::log(max-min));
    return {a,b,min,max,ll,0,std::isfinite(ll)?BetaStatus::Ok:BetaStatus::IterationFailure};
  }
  auto r=beta_mle(span{y.data(),y.size()},tol,maxit);
  r.logLik-=x.size()*std::log(max-min);
  return {r.alpha,r.beta,min,max,r.logLik,r.iterations,r.status};
}
}
