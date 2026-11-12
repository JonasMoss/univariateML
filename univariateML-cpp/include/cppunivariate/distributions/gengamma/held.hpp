// Conditional Prentice generalized-gamma likelihood calculations.
#pragma once

#include "../../core/span.hpp"
#include "../../core/work_budget.hpp"
#include "../trgamma/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace gengamma_fit_detail {

enum class ConditionalStatus { Ok, Invalid, Unbounded, IterationFailure };
struct ConditionalResult {
  double value;
  ConditionalStatus status;
};

// y contains log observations. For held sigma and Q, the location score is
// strictly decreasing and its zero is mean(y) + sigma * K(Q/sigma) / Q.
inline ConditionalResult location(span y, double sigma, double q) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (y.empty() || !(sigma > 0) || !std::isfinite(sigma) || !std::isfinite(q))
    return {nan, ConditionalStatus::Invalid};
  CPPUNIV_EXTENDED_REAL sum = 0;
  for (double v : y) {
    if (!std::isfinite(v)) return {nan, ConditionalStatus::Invalid};
    sum += v;
  }
  const double mean = static_cast<double>(sum / y.size());
  if (q == 0) return {mean, ConditionalStatus::Ok};
  std::vector<double> z;
  z.reserve(y.size());
  double maximum = -std::numeric_limits<double>::infinity();
  for (double v : y) {
    const double t = (q * ((v - mean) / sigma));
    if (!std::isfinite(t)) return {nan, ConditionalStatus::IterationFailure};
    z.push_back(t);
    maximum = std::max(maximum, t);
  }
  double k;
  if (maximum < 0.5 && *std::min_element(z.begin(), z.end()) > -0.5) {
    CPPUNIV_EXTENDED_REAL residual = 0;
    CPPUNIV_EXTENDED_REAL centered = 0;
    for (std::size_t i = 0; i < z.size(); ++i) {
      residual += trgamma_detail::expm1_minus(z[i]);
      centered += static_cast<CPPUNIV_EXTENDED_REAL>(y[i]) - mean;
    }
    const CPPUNIV_EXTENDED_REAL linear =
        (centered / y.size()) * (static_cast<CPPUNIV_EXTENDED_REAL>(q) / sigma);
    k = std::log1p(static_cast<double>(residual / y.size() + linear));
  } else {
    CPPUNIV_EXTENDED_REAL weights = 0;
    for (double t : z) weights += std::exp(t - maximum);
    k = maximum + std::log(static_cast<double>(weights / y.size()));
  }
  const double mu = mean + (k / q) * sigma;
  if (!std::isfinite(mu)) return {nan, ConditionalStatus::IterationFailure};
  return {mu, ConditionalStatus::Ok};
}

// With mu and Q held, the scale score is strictly decreasing in sigma.
// In w=(y-mu)/sigma it is -1+mean[w^2 exprel(Q*w)].
inline ConditionalResult scale(span y, double mu, double q, double tol, int maxit) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (y.empty() || !std::isfinite(mu) || !std::isfinite(q) ||
      !(tol > 0) || !std::isfinite(tol) || maxit < 1)
    return {nan, ConditionalStatus::Invalid};
  double maxdev = 0;
  for (double v : y) {
    if (!std::isfinite(v)) return {nan, ConditionalStatus::Invalid};
    maxdev = std::max(maxdev, std::abs(v - mu));
  }
  if (maxdev == 0) return {nan, ConditionalStatus::Unbounded};
  if (!std::isfinite(maxdev)) return {nan, ConditionalStatus::IterationFailure};
  auto score = [&](double logsigma) {
    const double sigma = std::exp(logsigma);
    if (sigma == 0) return std::numeric_limits<double>::infinity();
    if (!std::isfinite(sigma)) return -1.0;
    CPPUNIV_EXTENDED_REAL sum = 0;
    for (double v : y) {
      const double w = (v - mu) / sigma;
      const double z = q * w;
      if (!std::isfinite(w) || z > 700) return std::numeric_limits<double>::infinity();
      const double exprel = z == 0 ? 1 : std::expm1(z) / z;
      sum += static_cast<CPPUNIV_EXTENDED_REAL>(w) * w * exprel;
    }
    return static_cast<double>(sum / y.size() - 1);
  };
  double lo = std::log(maxdev), hi = lo;
  double flo = score(lo), fhi = flo;
  const int budget = static_cast<int>(solver_budget(maxit));
  for (int i = 0; i < budget && !(flo > 0 && fhi < 0); ++i) {
    if (!(flo > 0)) { lo -= 1; flo = score(lo); }
    if (!(fhi < 0)) { hi += 1; fhi = score(hi); }
  }
  if (!(flo > 0 && fhi < 0)) return {nan, ConditionalStatus::IterationFailure};
  for (int i = 0; i < budget; ++i) {
    const double mid = lo + (hi - lo) / 2;
    if (mid == lo || mid == hi || hi - lo <= 2 * tol) {
      if (hi - lo > 2 * tol) return {nan, ConditionalStatus::IterationFailure};
      const double out = std::exp(mid);
      return std::isfinite(out) && out > 0 ? ConditionalResult{out, ConditionalStatus::Ok}
                                           : ConditionalResult{nan, ConditionalStatus::IterationFailure};
    }
    const double f = score(mid);
    if (std::isnan(f)) return {nan, ConditionalStatus::IterationFailure};
    if (f > 0) lo = mid; else if (f < 0) hi = mid;
    else {
      const double a = std::max(lo, mid - tol/2), b = std::min(hi, mid + tol/2);
      if (!(a < mid && mid < b && score(a) > 0 && score(b) < 0))
        return {nan, ConditionalStatus::IterationFailure};
      lo = a; hi = b;
    }
  }
  return {nan, ConditionalStatus::IterationFailure};
}

}  // namespace gengamma_fit_detail
}  // namespace cppunivariate
