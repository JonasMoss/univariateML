// Fixed-Q joint location/scale profile.
#pragma once
#include "held.hpp"

namespace cppunivariate {
namespace gengamma_fit_detail {

struct JointResult {
  double mu, sigma;
  ConditionalStatus status;
  StopDetail stop_detail{};
};

// In standardized reflected data, t=|Q|*sd/sigma and the profiled
// scale score changes sign at t*K'(t)=Q^2. The left side is strictly
// increasing because K' >= 0 and K'' >= 0. The profiled location is
// monotone in sigma: its derivative has the sign of (K-t*K')/Q,
// and K-t*K' <= 0. Endpoint locations therefore enclose the solution.
inline JointResult location_scale(span y, double q, double tol, int maxit) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (y.empty() || !std::isfinite(q) || !(tol > 0) || !std::isfinite(tol) || maxit < 1)
    return {nan, nan, ConditionalStatus::Invalid};
  std::vector<double> reflected;
  reflected.reserve(y.size());
  for (double v : y) {
    if (!std::isfinite(v)) return {nan, nan, ConditionalStatus::Invalid};
    reflected.push_back(q < 0 ? -v : v);
  }
  trgamma_detail::LogData d;
  if (!trgamma_detail::standardize(span{reflected.data(), reflected.size()}, d)) {
    const bool constant = std::all_of(y.begin(), y.end(), [&](double v) { return v == y[0]; });
    return {nan, nan, constant ? ConditionalStatus::Unbounded : ConditionalStatus::IterationFailure};
  }
  if (q == 0) return {d.mean, d.sd, ConditionalStatus::Ok};
  const double absq = std::abs(q);
  auto score = [&](double logsigma) {
    const double sigma = std::exp(logsigma);
    if (sigma == 0) return std::numeric_limits<double>::infinity();
    if (!std::isfinite(sigma)) return -1.0;
    const double ratio = d.sd / sigma;
    const double t = absq * ratio;
    if (!std::isfinite(t) || !std::isfinite(ratio)) return std::numeric_limits<double>::infinity();
    if (t == 0) return ratio * ratio - 1;
    const auto c = trgamma_detail::cgf(d, t);
    return ratio * ratio * (c.K1 / t) - 1;
  };
  double lo = std::log(d.sd), hi = lo;
  double flo = score(lo), fhi = flo;
  const int budget = static_cast<int>(solver_budget(maxit));
  for (int i = 0; i < budget && !(flo > 0 && fhi < 0); ++i) {
    if (!(flo > 0)) { lo -= 1; flo = score(lo); }
    if (!(fhi < 0)) { hi += 1; fhi = score(hi); }
  }
  if (!(flo > 0 && fhi < 0)) return {nan, nan, ConditionalStatus::IterationFailure};
  for (int i = 0; i < budget; ++i) {
    const double mid = lo + (hi - lo) / 2;
    if (mid == lo || mid == hi || hi - lo <= tol) {
      if (hi - lo > tol) {
        const double value=std::exp(mid);const auto m=location(y,value,q);
        return {m.value,value,ConditionalStatus::IterationFailure,
          conditional_accuracy(y,m.value,value,q,true,tol,coefficient_spacing(mid,1.))};
      }
      const double sigma = std::exp(mid);
      const auto center = location(y, sigma, q);
      const auto left = location(y, std::exp(lo), q);
      const auto right = location(y, std::exp(hi), q);
      if (!(sigma > 0) || !std::isfinite(sigma) || center.status != ConditionalStatus::Ok ||
          left.status != ConditionalStatus::Ok || right.status != ConditionalStatus::Ok)
        return {nan, nan, ConditionalStatus::IterationFailure};
      if(std::abs(left.value-right.value)>tol*std::max(sigma,std::abs(center.value)))
        return {center.value,sigma,ConditionalStatus::IterationFailure,
          conditional_accuracy(y,center.value,sigma,q,true,tol,0)};
      return {center.value, sigma, ConditionalStatus::Ok};
    }
    const double f = score(mid);
    if (std::isnan(f)) return {nan, nan, ConditionalStatus::IterationFailure};
    if (f > 0) lo = mid; else if (f < 0) hi = mid;
    else {
      const double a = std::max(lo, mid - tol/4), b = std::min(hi, mid + tol/4);
      if (!(a < mid && mid < b && score(a) > 0 && score(b) < 0)) {
        const double value=std::exp(mid);const auto m=location(y,value,q);
        return {m.value,value,ConditionalStatus::IterationFailure,
          conditional_accuracy(y,m.value,value,q,true,tol,4*coefficient_spacing(mid,1.))};
      }
      lo = a; hi = b;
    }
  }
  return {nan, nan, ConditionalStatus::IterationFailure};
}

}  // namespace gengamma_fit_detail
}  // namespace cppunivariate
