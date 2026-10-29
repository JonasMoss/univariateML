#pragma once
#include "../lomax/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct Pareto2Result { double shape,scale,min,logLik; int iterations; LomaxStatus status; };

inline Pareto2Result pareto2_mle(span x, double min, double tol, int maxit) {
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  auto r=lomax_mle(span{y.data(),y.size()},tol,maxit);
  return {r.kappa,1/r.lambda,min,r.logLik,r.iterations,r.status};
}
}
