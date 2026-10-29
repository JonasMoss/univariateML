#pragma once
#include "../beta/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct NsbetaResult { double shape1,shape2,min,max,logLik; int iterations; BetaStatus status; };

inline NsbetaResult nsbeta_mle(span x, double min, double max, double tol, int maxit) {
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min)/(max-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min) && std::isfinite(max) && max>min;
  for(double v:x) valid=valid && std::isfinite(v) && v>min && v<max;
  if(!valid) y.clear();
  auto r=beta_mle(span{y.data(),y.size()},tol,maxit);
  r.logLik-=x.size()*std::log(max-min);
  return {r.alpha,r.beta,min,max,r.logLik,r.iterations,r.status};
}
}
