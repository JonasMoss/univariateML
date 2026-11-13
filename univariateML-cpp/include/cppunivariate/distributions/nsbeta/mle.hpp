#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../beta/mle.hpp"
#include <vector>
#include <cmath>
namespace cppunivariate {
struct NsbetaResult { double shape1,shape2,min,max,logLik; int iterations; BetaStatus status; };

inline NsbetaResult nsbeta_mle(span x, double min, double max, double tol, int maxit, double a=std::numeric_limits<double>::quiet_NaN(), double b=std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  for (std::size_t i=0;i<x.size();++i) y[i]=(x[i]-min)/(max-min);
  bool valid=tol>0 && std::isfinite(tol) && maxit>0 && std::isfinite(min) && std::isfinite(max) && max>min;
  for(double v:x) valid=valid && std::isfinite(v) && v>min && v<max;
  if(!valid) y.clear();
  if(!std::isnan(a)||!std::isnan(b)){
    const double nan=std::numeric_limits<double>::quiet_NaN();
    for(double v:{a,b})if(!std::isnan(v)&&(!(v>0)||!std::isfinite(v)))valid=false;
    if(!valid||x.size()==0)return {a,b,min,max,nan,0,BetaStatus::IterationFailure};
    // The all-held evaluator retains its existing arithmetic/status path.
    // A single free shape uses beta's stable monotone score below.
    if(!std::isnan(a) && !std::isnan(b)) {
      double s=0,r=0;
      for(double v:y){s+=std::log(v);r+=std::log1p(-v);}
      const double n=static_cast<double>(x.size());
      const double ll=(a-1)*s+(b-1)*r+n*(special::lgamma(a+b)-special::lgamma(a)-special::lgamma(b)-std::log(max-min));
      return {a,b,min,max,ll,0,std::isfinite(ll)?BetaStatus::Ok:BetaStatus::IterationFailure};
    }
    // Score values and the small-increment recurrence must retain enough
    // relative signal to verify a shape, rather than merely a likelihood.
    double target=0;
    for(double v:y)target-=std::isnan(a)?std::log(v):std::log1p(-v);
    target/=static_cast<double>(x.size());
    if(!(target>128*std::numeric_limits<double>::denorm_min()/1e-10))
      return {a,b,min,max,nan,0,BetaStatus::IterationFailure};
  }
  auto r=beta_mle(span{y.data(),y.size()},tol,maxit,a,b);
  if(r.status==BetaStatus::Ok && (!std::isnan(a)||!std::isnan(b))) {
    const double other=std::isnan(a)?b:a,free=std::isnan(a)?r.alpha:r.beta;
    if(free<20 && other<.01*free &&
        other/(free+other)<128*std::numeric_limits<double>::denorm_min()/1e-10)
      r.status=BetaStatus::IterationFailure;
  }
  r.logLik-=x.size()*std::log(max-min);
  if(r.status==BetaStatus::Ok && !std::isfinite(r.logLik))r.status=BetaStatus::IterationFailure;
  return {r.alpha,r.beta,min,max,r.logLik,r.iterations,r.status};
}

inline FitOutcome outcome(const NsbetaResult& r) {
  switch (r.status) {
    case BetaStatus::Ok: return FitOutcome::estimate(true);
    case BetaStatus::Degenerate: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case BetaStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case BetaStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
  }
  return {};
}


}
