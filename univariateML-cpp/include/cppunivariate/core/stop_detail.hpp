#pragma once
#include <cmath>
#include <vector>
namespace cppunivariate {
// Coefficients use the family's public order; face candidates use their model's.
enum class StopModel { None, Weibull, Pareto, InvWeibull, Power, Exp, InvExp };
struct FitCandidate {
  std::vector<double> coef;
  double loglik_lower = NAN, loglik_upper = NAN;
  StopModel model = StopModel::None;
};
struct StopDetail {
  std::vector<FitCandidate> candidates;
  std::vector<double> limit_coef;
  double limit_loglik = NAN;
  // Bounds on the global supremum, when available; infinity is intentional.
  double loglik_lower = NAN, loglik_upper = NAN;
  // Last unresolved interior segment, not a global bound.
  double segment_loglik_upper = NAN;
};
} // namespace cppunivariate
