#pragma once
#include "../gb2/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct FparetoResult { double shape1,shape2,shape3,scale,min,logLik; int iterations; Gb2Status status; };

inline FparetoResult fpareto_mle(span x, double min, double tol, int maxit, double shape1=std::numeric_limits<double>::quiet_NaN(), double shape2=std::numeric_limits<double>::quiet_NaN(), double shape3=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  auto r=gb2_mle(span{y.data(),y.size()},tol,maxit,shape1,shape2,shape3,scale);
  return {r.shape1,r.shape2,r.shape3,r.scale,min,r.logLik,r.iterations,r.status};
}
}
