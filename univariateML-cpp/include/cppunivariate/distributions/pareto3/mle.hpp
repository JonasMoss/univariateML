#pragma once
#include "../../core/work_budget.hpp"
#include "../burr/mle.hpp"
#include "../llogis/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
enum class Pareto3Status { Ok, InvalidData, Unbounded, IterationFailure, AccuracyFailure };
struct Pareto3Result { double shape,scale,min,logLik; int iterations; Pareto3Status status; bool certified=true; };

inline Pareto3Result pareto3_accuracy_search(span x, double min, double tol, int maxit, double shape=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
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

#include "dpqr.hpp"
#include "../../core/likelihood_polish.hpp"
namespace cppunivariate {
inline Pareto3Result pareto3_mle(span x,double min,double tol,int maxit,double shape=NAN,double scale=NAN) {
  auto r=pareto3_accuracy_search(x,min,tol,maxit,shape,scale);
  if(r.status!=Pareto3Status::Ok)return r;
  const auto parent=r;
  const double unit=r.scale;
  std::array<double,3> q{{std::log(r.shape),0,0}};
  const std::array<bool,3> free{{std::isnan(shape),std::isnan(scale),false}};
  const auto objective=[&](const std::array<accuracy_detail::Jet,3>& z) {
    const auto a=accuracy_detail::exp(z[0]);accuracy_detail::JetSum f;
    for(double v:x){const auto t=a*(accuracy_detail::Jet(std::log((static_cast<ExtendedReal>(v)-min)/unit))-z[1]);
      f.add(z[0]+t-accuracy_detail::Jet(2)*accuracy_detail::softplus(t));}
    return f.value;
  };
  if(!accuracy_detail::polish(q,free,tol,maxit,objective)){if(!parent.certified)return parent;r.status=Pareto3Status::IterationFailure;return r;}
  if(free[0])r.shape=std::exp(q[0]);
  if(free[1])r.scale=unit*std::exp(q[1]);
  if(free[1]&&!accuracy_detail::scale_projection(r.scale,unit,q[1],tol)){if(!parent.certified)return parent;r.status=Pareto3Status::AccuracyFailure;return r;}
  if(!parent.certified) {
    if(!(r.shape>0 && r.scale>0) || !std::isfinite(r.shape+r.scale))return parent;
    transformed_detail::Sum ll;
    for(double v:x) {
      const double y=v-min,t=r.shape*transformed_detail::log_ratio(y,r.scale);
      ll.push(std::log(r.shape)-std::log(y)-transformed_detail::softplus(t)-transformed_detail::softplus(-t));
    }
    r.logLik=ll.get();
    return std::isfinite(r.logLik) && r.logLik>=parent.logLik?r:parent;
  }
  r.logLik=0;for(double v:x)r.logLik+=distributions::dpareto3(v,r.shape,r.scale,min,true);
  return r;
}
}
