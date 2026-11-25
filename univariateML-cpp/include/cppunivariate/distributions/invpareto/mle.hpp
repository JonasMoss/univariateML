// Actuar (shape, scale) maps to Lomax (kappa, lambda) on 1/x.
#pragma once
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../lomax/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
// Exp on the reciprocal is inverse exponential with scale=rate.
inline StopDetail invpareto_stop_detail(StopDetail d,double jac) {
 for(auto& c:d.candidates) {
  if(c.model==StopModel::None && c.coef.size()==2)c.coef={c.coef[1],c.coef[0]};
  else if(c.model==StopModel::Exp && c.coef.size()==1){c.model=StopModel::InvExp;c.coef={c.coef[0]};}
  c.loglik_lower+=jac;c.loglik_upper+=jac;
 }
 d.limit_loglik+=jac;d.loglik_lower+=jac;d.loglik_upper+=jac;d.segment_loglik_upper+=jac;
 return d;
}

struct InvParetoResult { double shape, scale; double logLik; int iterations; LomaxStatus status; StopDetail stop_detail{}; LomaxCertificateFailure certificate_failure = LomaxCertificateFailure::None; bool limit_certified = false; bool singleton_proved = false; };
inline InvParetoResult invpareto_mle(span x, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v) || !std::isfinite(1/v)) {
      const double nan=std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,nan,0,LomaxStatus::InvalidStart};
    }
  }
  std::vector<double> y(x.size());
  double jacobian = 0;
  for (std::size_t i=0; i<x.size(); ++i) { y[i]=1.0/x[i]; jacobian -= 2*std::log(x[i]); }
  auto r = lomax_mle(span{y.data(), y.size()}, tol, maxit);
  return {r.kappa, r.lambda, r.logLik+jacobian, r.iterations, r.status,invpareto_stop_detail(r.stop_detail,jacobian),r.certificate_failure,
    r.limit_certified && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})};
}

inline InvParetoResult invpareto_mle(span x,double tol,int maxit,double shape,double scale) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(shape) && std::isnan(scale))return invpareto_mle(x,tol,maxit);
  std::vector<double> y;double jac=0;
  for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
  const auto r=lomax_mle(span{y.data(),y.size()},tol,maxit,scale,shape);
  return {r.kappa,r.lambda,r.logLik+jac,r.iterations,r.status,invpareto_stop_detail(r.stop_detail,jac),r.certificate_failure,
    r.limit_certified && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}),r.singleton_proved};
}

} // namespace cppunivariate

namespace cppunivariate {
inline FitOutcome outcome(const InvParetoResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  if (r.status == LomaxStatus::IterationFailure && r.certificate_failure == LomaxCertificateFailure::Budget) return FitOutcome::budget();
  if (r.status == LomaxStatus::IterationFailure && r.certificate_failure == LomaxCertificateFailure::Arithmetic) return FitOutcome::failure(OutcomeReason::Evaluation);
  switch (r.status) {
    case LomaxStatus::Ok: return r.singleton_proved ? FitOutcome::singleton(true) : FitOutcome::unknown_cardinality(true);
    case LomaxStatus::ExponentialLimit: return FitOutcome::limit(r.limit_certified);
    case LomaxStatus::InvalidStart: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case LomaxStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case LomaxStatus::AccuracyFailure: return FitOutcome::failure(OutcomeReason::Tolerance);
  }
  return {};
}
} // namespace cppunivariate
