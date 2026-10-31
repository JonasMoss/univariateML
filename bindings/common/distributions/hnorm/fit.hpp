#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/hnorm/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_hnorm(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const bool fixed = mu.supplied;
  const double held = fixed ? mu.value : std::numeric_limits<double>::quiet_NaN();
  if (fixed && !std::isfinite(held)) return fail("`mu` must be finite.");
  const double sc = sigma.supplied ? sigma.value : std::numeric_limits<double>::quiet_NaN();
  if (sigma.supplied && (!(sc>0) || !std::isfinite(sc))) return fail("`sigma` must be positive finite.");
  const auto res = cppunivariate::hnorm_mle(x, held, sc);
  if (sigma.supplied && res.status == cppunivariate::HnormStatus::Ok) {
    std::vector<std::string> fixed_names;
    if (fixed) fixed_names.push_back("mu");
    fixed_names.push_back("sigma");
    const double endpoint = res.mu;
    return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",endpoint},{"sigma",sc}}; out.logLik = res.logLik; out.fixed = fixed_names; out.has_fixed = true; return out; }();
  }
  if (fixed && res.status == cppunivariate::HnormStatus::DegenerateData)
    return fail("Half-normal likelihood is unbounded as sigma -> 0 when all observations equal held mu.");
  if (fixed && res.status != cppunivariate::HnormStatus::Ok)
    return fail("Half-normal observations must be finite and at least the held mu.");
  if (res.status == cppunivariate::HnormStatus::DegenerateData)
    return fail("Translated half-normal MLE requires at least two observations with distinct values; degenerate data have no positive-scale MLE.");
  if (res.status != cppunivariate::HnormStatus::Ok)
    return fail("Translated half-normal MLE requires finite data with a representable range.");
  if (fixed) return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",res.mu},{"sigma",res.sigma}}; out.logLik = res.logLik; out.fixed = std::vector<std::string>{"mu"}; out.has_fixed = true; return out; }();
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",res.mu},{"sigma",res.sigma}}; out.logLik = res.logLik; return out; }();

}
}
