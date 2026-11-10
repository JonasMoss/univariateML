#pragma once
#include "../../core/work_budget.hpp"
#include "../burr/mle.hpp"
#include "../llogis/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
enum class Pareto3Status { Ok, InvalidData, Unbounded, IterationFailure };
struct Pareto3Result { double shape,scale,min,logLik; int iterations; Pareto3Status status; bool certified=true; };

inline Pareto3Result pareto3_mle(span x, double min, double tol, int maxit, double shape=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min);
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  if(!valid) y.clear();
  auto r=loglogistic_mle(span{y.data(),y.size()},tol,maxit,shape,scale);
  const auto status=!valid || x.size()==0 || r.status==LogLogisticStatus::InvalidInput ? Pareto3Status::InvalidData :
    r.status==LogLogisticStatus::Unbounded ? Pareto3Status::Unbounded :
    r.status==LogLogisticStatus::Ok && std::isfinite(r.logLik) && r.shape>0 && r.scale>0 ? Pareto3Status::Ok : Pareto3Status::IterationFailure;
  return {r.shape,r.scale,min,r.logLik,r.iterations,status,r.certified};
}
}
