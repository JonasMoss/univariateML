#pragma once
#include "../../core/outcome.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/work_budget.hpp"
#include "../burr/mle.hpp"
#include "../llogis/mle.hpp"
#include <algorithm>
#include <vector>
#include <cmath>
namespace cppunivariate {
enum class Pareto3Status { Ok, InvalidData, Unbounded, IterationFailure, AccuracyFailure, Unsupported, TransformFailure };
struct Pareto3Result { double shape,scale,min,logLik; int iterations; Pareto3Status status; bool certified=true;  StopDetail stop_detail{}; };

inline FitOutcome outcome(const Pareto3Result& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case Pareto3Status::Unsupported: return FitOutcome::failure(OutcomeReason::Unsupported);
    case Pareto3Status::Ok: return FitOutcome::singleton(r.certified);
    case Pareto3Status::TransformFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
    case Pareto3Status::InvalidData: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case Pareto3Status::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case Pareto3Status::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case Pareto3Status::AccuracyFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}

inline Pareto3Result pareto3_accuracy_search(span x, double min, double tol, int maxit, double shape=std::numeric_limits<double>::quiet_NaN(), double scale=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min) &&
    (std::isnan(shape)||(shape>0 && std::isfinite(shape))) &&
    (std::isnan(scale)||(scale>0 && std::isfinite(scale)));
  for(double v:x) valid=valid && std::isfinite(v) && v>min;
  bool constant=!x.empty(),exact=true;
  if(valid)for(std::size_t i=0;i<x.size();++i) {
    constant=constant && x[i]==x[0];
    if(!std::isfinite(y[i])) {
      if(constant && std::isnan(shape) && std::isnan(scale) &&
         std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
        return {NAN,NAN,min,NAN,0,Pareto3Status::Unbounded};
      return {NAN,NAN,min,NAN,0,Pareto3Status::TransformFailure};
    }
    const double virtual_min=x[i]-y[i];
    const double error=(x[i]-(y[i]+virtual_min))+(virtual_min-min);
    exact=exact && error==0;
  }
  if(!valid) y.clear();
  auto r=loglogistic_mle(span{y.data(),y.size()},tol,maxit,shape,scale);
  const auto status=!valid || x.size()==0 || r.status==LogLogisticStatus::InvalidInput ? Pareto3Status::InvalidData :
    r.status==LogLogisticStatus::Unbounded ?
      (constant && (std::isnan(scale)||exact) ? Pareto3Status::Unbounded : Pareto3Status::TransformFailure) :
    r.status==LogLogisticStatus::Ok && std::isfinite(r.logLik) && r.shape>0 && r.scale>0 ? Pareto3Status::Ok : Pareto3Status::IterationFailure;
  return {r.shape,r.scale,min,r.logLik,r.iterations,status,r.certified};
}
}

#include "dpqr.hpp"
#include "../../core/likelihood_polish.hpp"
namespace cppunivariate {
inline Pareto3Result pareto3_mle(span x,double min,double tol,int maxit,double shape=NAN,double scale=NAN) {
  if(std::isnan(min))return {NAN,NAN,min,NAN,0,Pareto3Status::Unsupported};
  auto r=pareto3_accuracy_search(x,min,tol,maxit,shape,scale);
  if(r.status!=Pareto3Status::Ok)return r;
  if(!r.certified)tol=std::min(tol,1e-10);
  const auto parent=r;
  const double unit=r.scale;
  const std::array<bool,3> free{{std::isnan(shape),std::isnan(scale),false}};
  double ref=x[0];
  if(free[1]) {
    std::vector<double> ordered(x.begin(),x.end());
    const auto median=ordered.begin()+ordered.size()/2;
    std::nth_element(ordered.begin(),median,ordered.end());
    ref=*median;
  }
  const double anchor=ref-min, virtual_min=anchor-ref;
  const double residual=(ref-(anchor-virtual_min))+(-min-virtual_min);
  const ExtendedReal excess=anchor;
  const ExtendedReal offset=free[1]?burr_excess_log_ratio(ref,min,unit):0;
  // A held scale already gives a fixed origin. With scale free, work in
  // log excess relative to a sample point and translate the fitted location.
  std::array<double,3> q{{std::log(r.shape),static_cast<double>(-offset),0}};
  std::vector<ExtendedReal> logs;
  logs.reserve(x.size());
  for(double v:x) {
    const double delta=v-ref, virtual_ref=delta-v;
    const double delta_residual=(v-(delta-virtual_ref))+(-ref-virtual_ref);
    const ExtendedReal relative=delta/excess;
    const ExtendedReal correction=delta_residual/excess-relative*(residual/excess);
    logs.push_back(free[1] && std::fabs(relative)<0.5
      ? std::log1p(relative+correction)
      : burr_excess_log_ratio(v,min,unit)-offset);
  }
  const auto objective=[&](const std::array<accuracy_detail::Jet,3>& z) {
    const auto a=accuracy_detail::exp(z[0]);accuracy_detail::JetSum f;
    for(ExtendedReal l:logs){
      const auto t=a*(accuracy_detail::Jet(l)-z[1]);
      f.add(z[0]+t-accuracy_detail::Jet(2)*accuracy_detail::softplus(t));
    }
    return f.value;
  };
  const bool refined=parent.certified?accuracy_detail::polish(q,free,tol,maxit,objective,r.stop_detail):continuous_fallback_polish(q,free,tol,maxit,objective,r.stop_detail);
  if(!refined){r.status=Pareto3Status::IterationFailure;{if(r.shape>0 && r.scale>0)record_accuracy(r,tol,{r.shape,r.scale,r.min},{std::isnan(shape)?r.shape:0,std::isnan(scale)?r.scale:0,0});else{r.stop_detail.accuracy_stop=AccuracyStop::None;r.stop_detail.attainable_tol=NAN;}}return r;}
  if(free[0])r.shape=std::exp(q[0]);
  if(free[1])q[1]=static_cast<double>(q[1]+offset);
  if(free[1])r.scale=unit*std::exp(q[1]);
  if(free[1]&&!accuracy_detail::scale_projection(r.scale,unit,q[1],tol)){{r.status=Pareto3Status::AccuracyFailure;{if(r.shape>0 && r.scale>0)record_accuracy(r,tol,{r.shape,r.scale,r.min},{std::isnan(shape)?r.shape:0,std::isnan(scale)?r.scale:0,0});else{r.stop_detail.accuracy_stop=AccuracyStop::None;r.stop_detail.attainable_tol=NAN;}}}return r;}
  if(!parent.certified) {
    if(!(r.shape>0 && r.scale>0) || !std::isfinite(r.shape+r.scale)){{r.status=Pareto3Status::AccuracyFailure;{if(r.shape>0 && r.scale>0)record_accuracy(r,tol,{r.shape,r.scale,r.min},{std::isnan(shape)?r.shape:0,std::isnan(scale)?r.scale:0,0});else{r.stop_detail.accuracy_stop=AccuracyStop::None;r.stop_detail.attainable_tol=NAN;}}}return r;}
    transformed_detail::Sum ll;
    for(double v:x) {
      const double y=v-min,t=r.shape*burr_excess_log_ratio(v,min,r.scale);
      ll.push(std::log(r.shape)-std::log(y)-transformed_detail::softplus(t)-transformed_detail::softplus(-t));
    }
    r.logLik=ll.get();
    if(!std::isfinite(r.logLik)){r.status=Pareto3Status::AccuracyFailure;{if(r.shape>0 && r.scale>0)record_accuracy(r,tol,{r.shape,r.scale,r.min},{std::isnan(shape)?r.shape:0,std::isnan(scale)?r.scale:0,0});else{r.stop_detail.accuracy_stop=AccuracyStop::None;r.stop_detail.attainable_tol=NAN;}}}
    return r;
  }
  r.logLik=0;for(double v:x)r.logLik+=distributions::dpareto3(v,r.shape,r.scale,min,true);
  return r;
}
}
