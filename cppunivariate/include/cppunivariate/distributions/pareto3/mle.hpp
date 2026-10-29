#pragma once
#include "../burr/mle.hpp"
#include "../llogis/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
enum class Pareto3Status { Ok, InvalidData, Unbounded, IterationFailure };
struct Pareto3Result { double shape,scale,min,logLik; int iterations; Pareto3Status status; };

inline Pareto3Result pareto3_mle(span x, double min, double tol, int maxit, double shape=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  if(!std::isnan(shape)||!std::isnan(scale)){
    auto r=burr_mle_holds(span{y.data(),y.size()},1,shape,scale,tol,maxit);
    return {r.shape2,r.scale,min,r.logLik,r.iterations,r.status==BurrStatus::Ok?Pareto3Status::Ok:r.status==BurrStatus::Unbounded?Pareto3Status::Unbounded:Pareto3Status::IterationFailure};
  }
  auto r=loglogistic_mle(span{y.data(),y.size()},tol,maxit);
  bool constant=x.size()>0;
  for(double v:x) constant=constant && v==x[0];
  const auto status=!valid || x.size()==0 ? Pareto3Status::InvalidData :
    (constant ? Pareto3Status::Unbounded :
      (r.status==LogLogisticStatus::Ok && std::isfinite(r.logLik) && r.shape>0 && r.rate>0 ? Pareto3Status::Ok : Pareto3Status::IterationFailure));
  return {r.shape,1/r.rate,min,r.logLik,r.iterations,status};
}
}
