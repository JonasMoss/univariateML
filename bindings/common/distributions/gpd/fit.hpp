#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gpd/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_gpd(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held xi_arg = {}, double reltol = 1e-12, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;
  const Held xi = xi_arg;

  if (!mu.supplied) return fail("Estimating `mu` is not supported: the free-threshold GPD likelihood has no MLE (endpoint and logistic escapes). Supply a held threshold.");
  const double threshold = mu.value;
  const double shape = xi.supplied ? xi.value :
      std::numeric_limits<double>::quiet_NaN();
  if (xi.supplied && (!std::isfinite(shape) || (shape < -1 && !sigma.supplied)))
    return fail("`xi` must be finite and at least -1.");
  const double scale=sigma.supplied ? sigma.value : std::numeric_limits<double>::quiet_NaN();
  if(sigma.supplied && (!(scale>0) || !std::isfinite(scale))) return fail("`sigma` must be positive and finite.");
  std::vector<std::string> fixed{"mu"};
  if(sigma.supplied) fixed.push_back("sigma");
  if(xi.supplied) fixed.push_back("xi");
  auto res = cppunivariate::gpd_mle(x, reltol, iterlim, threshold, shape, scale);
  if (res.status == cppunivariate::GpdStatus::InvalidData)
    return fail("Generalized Pareto requires finite data at or above `mu`, compatible support and holds, no scale collapse at the threshold, a finite threshold, positive `reltol` and positive `iterlim`.");
  if (res.status != cppunivariate::GpdStatus::Ok || !std::isfinite(res.logLik))
    return fail("Generalized Pareto MLE failed to converge.");
  std::vector<std::pair<std::string,std::string>> restricted;
  if(res.restricted & 1u) restricted.emplace_back("mu","lower");
  if(res.restricted & 2u) restricted.emplace_back("mu","upper");
  if(res.restricted & 4u) restricted.emplace_back("sigma","lower");
  if(res.restricted & 8u) restricted.emplace_back("sigma","upper");
  if(res.restricted & 16u) restricted.emplace_back("xi","lower");
  if(res.restricted & 32u) restricted.emplace_back("xi","upper");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",res.mu},{"sigma",res.sigma},{"xi",res.xi}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
