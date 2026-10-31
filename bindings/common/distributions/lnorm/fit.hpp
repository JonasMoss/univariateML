#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include <limits>
#include <cmath>
namespace umvl_binding {
inline FitResult fit_lnorm(cppunivariate::span x, Held meanlog_arg = {}, Held sdlog_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held meanlog = meanlog_arg;
  const Held sdlog = sdlog_arg;

  std::vector<std::string> fixed;
  const double meanlog_held = !meanlog.supplied ? std::numeric_limits<double>::quiet_NaN() : meanlog.value;
  if (meanlog.supplied) {
    if (!(std::isfinite(meanlog_held))) return fail("Held `meanlog` must be finite.");
    fixed.push_back("meanlog");
  }
  const double sdlog_held = !sdlog.supplied ? std::numeric_limits<double>::quiet_NaN() : sdlog.value;
  if (sdlog.supplied) {
    if (!(std::isfinite(sdlog_held) && sdlog_held > 0.0)) return fail("Held `sdlog` must be positive and finite.");
    fixed.push_back("sdlog");
  }

  auto res = cppunivariate::lognormal_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, meanlog_held, sdlog_held);
  if (res.status == cppunivariate::LognormalStatus::Unbounded) {
    return fail("The lnorm maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as scale shrinks to zero.");
  }
  if (res.status == cppunivariate::LognormalStatus::InvalidInput) return fail("Lognormal requires nonempty positive finite data and valid held parameters.");
  if (res.status != cppunivariate::LognormalStatus::Ok) return fail("Lognormal likelihood exceeds double precision.");
  if (!std::isfinite(res.meanlog) || !std::isfinite(res.logLik)) return fail("Lognormal likelihood exceeds double precision.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"meanlog",res.meanlog},{"sdlog",res.sdlog}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
