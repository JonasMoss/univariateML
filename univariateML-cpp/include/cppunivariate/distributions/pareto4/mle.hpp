#pragma once
#include "../burr/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct Pareto4Result { double shape1,shape2,scale,min,logLik; int iterations; BurrStatus status; };

inline Pareto4Result pareto4_mle_certified(span x, double min, double tol, int maxit);

inline Pareto4Result pareto4_mle(span x, double min, double tol, int maxit, double scale=std::numeric_limits<double>::quiet_NaN(), double shape1=std::numeric_limits<double>::quiet_NaN(), double shape2=std::numeric_limits<double>::quiet_NaN()) {
  if(std::isnan(scale) && std::isnan(shape1) && std::isnan(shape2))
    return pareto4_mle_certified(x,min,tol,maxit);
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  auto r=(!std::isnan(shape1)||!std::isnan(shape2)) ? burr_mle_holds(span{y.data(),y.size()},shape1,shape2,scale,tol,maxit) : std::isnan(scale) ? burr_mle_legacy(span{y.data(),y.size()},tol,maxit) : burr_mle_fixed_scale(span{y.data(),y.size()},scale,tol,maxit);
  return {r.shape1,r.shape2,r.scale,min,r.logLik,r.iterations,r.status};
}
// Certified joint excess fit shared by the public all-free regular path.
inline Pareto4Result pareto4_mle_certified(span x, double min, double tol, int maxit) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(!std::isfinite(min))
    return {nan,nan,nan,min,nan,0,BurrStatus::InvalidInit};
  std::vector<double> y;
  for(double v:x) {
    if(!std::isfinite(v)||!(v>min))
      return {nan,nan,nan,min,nan,0,BurrStatus::InvalidInit};
    if(!std::isfinite(v-min))
      return {nan,nan,nan,min,nan,0,BurrStatus::NumericalFailure};
    y.push_back(v-min);
  }
  // Translation must not create artificial ties in the certificate input.
  std::vector<std::size_t> order;
  for(std::size_t i=0;i<x.size();++i)order.push_back(i);
  std::sort(order.begin(),order.end(),[&](std::size_t i,std::size_t j){return x[i]<x[j];});
  for(std::size_t i=1;i<order.size();++i)
    if(x[order[i]]!=x[order[i-1]] && y[order[i]]==y[order[i-1]])
      return {nan,nan,nan,min,nan,0,BurrStatus::NumericalFailure};
  const auto r=burr_mle_certified(span{y.data(),y.size()},tol,maxit);
  return {r.shape1,r.shape2,r.scale,min,r.logLik,r.iterations,r.status};
}

}
