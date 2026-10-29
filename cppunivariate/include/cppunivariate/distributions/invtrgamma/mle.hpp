// Inverse transformed gamma MLE: X ~ invtrgamma(shape1, shape2, scale)
// iff 1/X ~ trgamma(shape1, shape2, 1/scale). The transformed gamma fit
// runs on log(1/x) = -log(x); the log likelihood differs by the Jacobian
// -2 sum(log(x)). Faces mirror: lognormal stays lognormal, the power face
// with endpoint 1/min(x) becomes Pareto with minimum min(x).
#pragma once

#include "../trgamma/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class InvTrGammaStatus {
  Ok,
  LognormalBoundary,
  ParetoBoundary,
  ScaleOutOfRange,
  InvalidData,
  InvalidFixed,
  IterationFailure
};

struct InvTrGammaResult {
  double shape1, shape2, scale;
  double log_scale;
  double logLik;
  int iterations;
  InvTrGammaStatus status;
};

inline InvTrGammaStatus invtrgamma_status(TrGammaStatus s) {
  switch (s) {
    case TrGammaStatus::Ok: return InvTrGammaStatus::Ok;
    case TrGammaStatus::LognormalBoundary: return InvTrGammaStatus::LognormalBoundary;
    case TrGammaStatus::PowerBoundary: return InvTrGammaStatus::ParetoBoundary;
    case TrGammaStatus::ScaleOutOfRange: return InvTrGammaStatus::ScaleOutOfRange;
    case TrGammaStatus::InvalidData: return InvTrGammaStatus::InvalidData;
    case TrGammaStatus::InvalidFixed: return InvTrGammaStatus::InvalidFixed;
    case TrGammaStatus::IterationFailure: return InvTrGammaStatus::IterationFailure;
  }
  return InvTrGammaStatus::IterationFailure;
}

inline InvTrGammaResult invtrgamma_mle(span x, double tol = 1e-10, int maxit = 100,
                                       double shape1 = std::numeric_limits<double>::quiet_NaN(),
                                       double shape2 = std::numeric_limits<double>::quiet_NaN()) {
  std::vector<double> y(x.size());
  double jacobian = 0.0;
  for (std::size_t i = 0; i < x.size(); ++i) {
    if (!(x[i] > 0.0) || !std::isfinite(x[i])) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      return {shape1, shape2, nan, nan, nan, 0, InvTrGammaStatus::InvalidData};
    }
    y[i] = -std::log(x[i]);
    jacobian += 2.0 * y[i];
  }
  const TrGammaResult r = trgamma_fit_log(span{y.data(), y.size()}, tol, maxit, shape1, shape2);
  const double log_scale = -r.log_scale;
  const TrGammaStatus status = trgamma_range_status(r.status, log_scale);
  return {r.shape1, r.shape2, std::exp(log_scale), log_scale, r.logLik + jacobian,
          r.iterations, invtrgamma_status(status)};
}

}  // namespace cppunivariate
