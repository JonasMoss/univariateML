#pragma once
#include "../../core/work_budget.hpp"
#include "../gb2/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct FparetoResult { double shape1,shape2,shape3,scale,min,logLik; int iterations; Gb2Status status; StopDetail stop_detail{}; bool certified=true; };

inline FparetoResult fpareto_mle(span x, double min, double tol, int maxit, double shape1=std::numeric_limits<double>::quiet_NaN(), double shape2=std::numeric_limits<double>::quiet_NaN(), double shape3=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  const bool power_certificate=!std::isnan(shape2) && std::isnan(scale) &&
    (std::isnan(shape1)!=std::isnan(shape3)) && shape1!=1 && shape3!=1;
  if(valid && ((!std::isnan(shape1)&&!std::isnan(shape3)) || power_certificate || (!std::isnan(scale) && std::isnan(shape2) &&
     ((!std::isnan(shape1)&&std::isnan(shape3))||(!std::isnan(shape3)&&std::isnan(shape1)))))) {
    for(double v:y)if(!std::isfinite(v))return {NAN,NAN,NAN,scale,min,NAN,0,Gb2Status::NumericalFailure};
    // A rounded equality at held scale must be an exact subtraction.
    // Error-free TwoSum residual for x[i] + (-min).
    for(std::size_t i=0;i<x.size();++i)if(y[i]==scale) {
      const double virtual_b=y[i]-x[i];
      const double residual=(x[i]-(y[i]-virtual_b))+(-min-virtual_b);
      if(!std::isfinite(residual)||residual!=0)return {NAN,NAN,NAN,scale,min,NAN,0,Gb2Status::NumericalFailure};
    }
    std::vector<std::size_t> order;
    for(std::size_t i=0;i<x.size();++i)order.push_back(i);
    std::sort(order.begin(),order.end(),[&](std::size_t i,std::size_t j){return x[i]<x[j];});
    for(std::size_t i=1;i<order.size();++i)
      if(x[order[i]]!=x[order[i-1]]&&y[order[i]]==y[order[i-1]])
        return {NAN,NAN,NAN,scale,min,NAN,0,Gb2Status::NumericalFailure};
  }
  auto r=gb2_mle(span{y.data(),y.size()},tol,maxit,shape1,shape2,shape3,scale);
  auto detail=r.stop_detail;
  for(auto& c:detail.candidates)if(c.model==StopModel::None && c.coef.size()==4)c.coef.push_back(min);
  return {r.shape1,r.shape2,r.shape3,r.scale,min,r.logLik,r.iterations,r.status,detail,r.certified};
}
}
