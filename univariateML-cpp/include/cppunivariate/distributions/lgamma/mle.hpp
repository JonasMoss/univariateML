// Log-gamma: Y=log(X) is gamma; log f_X = log f_Y - log(X).
// Preserve the gamma solver's status and held rate exactly.
#pragma once
#include "../../core/work_budget.hpp"
#include "../gamma/transformed.hpp"
#include <vector>

namespace cppunivariate {
enum class LgammaStatus { Ok, Degenerate, TransformFailure, IterationFailure };
struct LgammaResult {
  double shapelog, ratelog, logLik;
  int iterations;
  LgammaStatus status;
  bool certified=true;
};
inline LgammaResult lgamma_mle(span x, double tol, int maxit,
    double shapelog = std::numeric_limits<double>::quiet_NaN(),
    double ratelog = std::numeric_limits<double>::quiet_NaN()) {
  const auto r=transformed_detail::gamma_raw(x,tol,maxit,shapelog,ratelog,transformed_detail::GammaMap::Log);
  // Retain ordinary delegated results only after the raw sufficient-statistic
  // solve verifies their coefficients and likelihood to a tighter tolerance.
  // This preserves the established ordinary-data API without treating its
  // rounded transform as evidence on concentrated samples.
  if(r.status==GammaStatus::Ok) {
    std::vector<double> y;double jac=0,spread=0;
    for(double v:x){y.push_back(std::log(v));jac+=y.back();spread=std::max(spread,std::abs(transformed_detail::log_ratio(v,x[0])));}
    if(spread>1e-3) {
      const auto old=gamma_mle(span{y.data(),y.size()},tol,maxit,shapelog,std::isnan(ratelog)?NAN:1/ratelog);
      const double rate=std::isnan(ratelog)?1/old.scale:ratelog;
      const double allowance=std::min(tol,1e-12);
      if(old.status==GammaStatus::Ok && std::abs(old.shape/r.shape-1)<=allowance &&
         std::abs(rate/r.scale-1)<=allowance &&
         std::abs(old.logLik-jac-r.logLik)<=allowance*std::max(1.0,std::abs(r.logLik)))
        return {old.shape,rate,old.logLik-jac,old.iterations,LgammaStatus::Ok,r.certified};
    }
  }
  const auto status=r.status==GammaStatus::Ok?LgammaStatus::Ok:
    r.status==GammaStatus::Degenerate?LgammaStatus::Degenerate:
    r.status==GammaStatus::TransformFailure?LgammaStatus::TransformFailure:LgammaStatus::IterationFailure;
  return {r.shape,r.scale,r.logLik,r.iterations,status,r.certified};
}
} // namespace cppunivariate
