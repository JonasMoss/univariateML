#pragma once
#include "../burr/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct Pareto4Result { double shape1,shape2,scale,min,logLik; int iterations; BurrStatus status; };

inline Pareto4Result pareto4_mle(span x, double min, double tol, int maxit, double scale=std::numeric_limits<double>::quiet_NaN(), double shape1=std::numeric_limits<double>::quiet_NaN(), double shape2=std::numeric_limits<double>::quiet_NaN()) {
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  auto r=(!std::isnan(shape1)||!std::isnan(shape2)) ? burr_mle_holds(span{y.data(),y.size()},shape1,shape2,scale,tol,maxit) : std::isnan(scale) ? burr_mle_legacy(span{y.data(),y.size()},tol,maxit) : burr_mle_fixed_scale(span{y.data(),y.size()},scale,tol,maxit);
  return {r.shape1,r.shape2,r.scale,min,r.logLik,r.iterations,r.status};
}
}
