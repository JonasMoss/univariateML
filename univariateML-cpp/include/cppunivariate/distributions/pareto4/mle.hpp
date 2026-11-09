#pragma once
#include "../../core/work_budget.hpp"
#include "../burr/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct Pareto4Result { double shape1,shape2,scale,min,logLik; int iterations; BurrStatus status; StopDetail stop_detail{}; };
} // namespace cppunivariate
#include "centered.hpp"
namespace cppunivariate {

inline Pareto4Result pareto4_mle_certified(span x, double min, double tol, int maxit);

inline Pareto4Result pareto4_mle(span x, double min, double tol, int maxit, double scale=std::numeric_limits<double>::quiet_NaN(), double shape1=std::numeric_limits<double>::quiet_NaN(), double shape2=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(scale) && std::isnan(shape1) && std::isnan(shape2))
    return pareto4_mle_certified(x,min,tol,maxit);
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  if(valid && std::isnan(shape1) && std::isnan(shape2) && !std::isnan(scale)) {
    for(double v:y)if(!std::isfinite(v))return {NAN,NAN,scale,min,NAN,0,BurrStatus::NumericalFailure};
    // A rounded equality at held scale must be an exact subtraction.
    // Error-free TwoSum residual for x[i] + (-min).
    for(std::size_t i=0;i<x.size();++i)if(y[i]==scale) {
      const double virtual_b=y[i]-x[i];
      const double residual=(x[i]-(y[i]-virtual_b))+(-min-virtual_b);
      if(!std::isfinite(residual)||residual!=0)return {NAN,NAN,scale,min,NAN,0,BurrStatus::NumericalFailure};
    }
    std::vector<std::size_t> order;
    for(std::size_t i=0;i<x.size();++i)order.push_back(i);
    std::sort(order.begin(),order.end(),[&](std::size_t i,std::size_t j){return x[i]<x[j];});
    for(std::size_t i=1;i<order.size();++i)
      if(x[order[i]]!=x[order[i-1]]&&y[order[i]]==y[order[i-1]])
        return {NAN,NAN,scale,min,NAN,0,BurrStatus::NumericalFailure};
  }
  if(valid && shape1>0 && std::isfinite(shape1) && std::isnan(shape2) && std::isnan(scale) && !x.empty()) {
    double relative_spread=0;
    for(double v:x)relative_spread=std::max(relative_spread,std::abs((v-x[0])/y[0]));
    if(relative_spread>0 && relative_spread<=1e-3)
      return pareto4_detail::centered_shape(x,min,shape1,tol,maxit);
  }
  auto r=(!std::isnan(shape1)||!std::isnan(shape2)) ? burr_mle_holds(span{y.data(),y.size()},shape1,shape2,scale,tol,maxit) : std::isnan(scale) ? burr_mle_legacy(span{y.data(),y.size()},tol,maxit) : burr_mle_fixed_scale(span{y.data(),y.size()},scale,tol,maxit);
  auto detail=r.stop_detail;
  for(auto& c:detail.candidates) if(c.model==StopModel::None && c.coef.size()==3)c.coef.push_back(min);
  return {r.shape1,r.shape2,r.scale,min,r.logLik,r.iterations,r.status,detail};
}
// Certified joint excess fit shared by the public all-free regular path.
inline Pareto4Result pareto4_mle_certified(span x, double min, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
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
  auto detail=r.stop_detail;
  for(auto& c:detail.candidates) if(c.model==StopModel::None && c.coef.size()==3)c.coef.push_back(min);
  return {r.shape1,r.shape2,r.scale,min,r.logLik,r.iterations,r.status,detail};
}

}
