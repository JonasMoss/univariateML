#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/laplace/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_laplace(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mu=mu.supplied?mu.value:nan;
  if(mu.supplied && (!std::isfinite(held_mu)))return fail("`mu` must be finite.");
  const double held_sigma=sigma.supplied?sigma.value:nan;
  if(sigma.supplied && (!std::isfinite(held_sigma) || !(held_sigma>0)))return fail("`sigma` must be positive and finite.");
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");

  auto res = cppunivariate::laplace_mle(x,held_mu,held_sigma);
  if (!(res.sigma>0.0) || !std::isfinite(res.sigma))
    return fail("The Laplace maximum likelihood estimator does not exist for constant data; other data may exceed double precision.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",res.mu},{"sigma",res.sigma}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; return out; }();

}
}
