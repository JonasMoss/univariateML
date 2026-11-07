// Log-gamma: Y=log(X) is gamma; log f_X = log f_Y - log(X).
// Preserve the gamma solver's status and held rate exactly.
#pragma once
#include "../../core/work_budget.hpp"
#include "../gamma/mle.hpp"
#include <vector>

namespace cppunivariate {
enum class LgammaStatus { Ok, Degenerate, TransformFailure, IterationFailure };
struct LgammaResult {
  double shapelog, ratelog, logLik;
  int iterations;
  LgammaStatus status;
};
inline LgammaResult lgamma_mle(span x, double tol, int maxit,
    double shapelog = std::numeric_limits<double>::quiet_NaN(),
    double ratelog = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  double sum_y=0;
  bool constant=true;
  for(std::size_t i=0;i<x.size();++i) {
    y[i]=std::log(x[i]);
    sum_y+=y[i];
    constant=constant && x[i]==x[0];
  }
  const bool held=std::isfinite(ratelog);
  const auto r=gamma_mle(span{y.data(),y.size()},tol,maxit,shapelog,held ? 1.0/ratelog : ratelog);
  const auto status=r.status==GammaStatus::Ok ? LgammaStatus::Ok :
      r.status==GammaStatus::Degenerate ? (constant ? LgammaStatus::Degenerate : LgammaStatus::TransformFailure) :
      LgammaStatus::IterationFailure;
  return {r.shape,held ? ratelog : (std::isfinite(r.scale) ? 1.0/r.scale : r.scale),
          r.logLik-sum_y,r.iterations,status};
}
} // namespace cppunivariate
