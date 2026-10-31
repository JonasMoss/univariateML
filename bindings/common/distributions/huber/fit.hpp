#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/huber/mle.hpp"
#include <limits>
namespace umvl_binding {
inline FitResult fit_huber(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held epsilon_arg = {}, double gradtol = 1e-10, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;
  const Held epsilon = epsilon_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double m=mu.supplied ? mu.value : nan;
  const double s=sigma.supplied ? sigma.value : nan;
  if (mu.supplied && !std::isfinite(m)) return fail("`mu` must be finite.");
  if (sigma.supplied && (!(s>0) || !std::isfinite(s)))
    return fail("`sigma` must be positive and finite.");
  if (epsilon.supplied && epsilon.length!=1) return fail("`epsilon` must be a scalar or NULL.");
  const double e=epsilon.supplied ? epsilon.value : nan;
  if (epsilon.supplied && (!(e>0) || !std::isfinite(e)))
    return fail("`epsilon` must be positive and finite.");
  auto r=cppunivariate::huber_mle(x,gradtol,iterlim,e,m,s);
  if (r.status==cppunivariate::HuberStatus::InvalidData)
    return fail("Huber requires finite data, positive finite `gradtol` and positive `iterlim`.");
  if (r.status==cppunivariate::HuberStatus::UnboundedLikelihood)
    return fail("Huber likelihood is unbounded for constant data at the fitted or held location.");
  if (r.status==cppunivariate::HuberStatus::NormalLimit)
    return fail("Huber shape reaches the normal face; use `mlnorm`.");
  if (r.status==cppunivariate::HuberStatus::LaplaceLimit)
    return fail("Huber shape reaches the Laplace face; use `mllaplace`.");
  if (r.status!=cppunivariate::HuberStatus::Ok || !std::isfinite(r.logLik))
    return fail("Huber MLE failed to converge.");
  std::vector<std::string> fixed;
  if (epsilon.supplied) fixed.push_back("epsilon");
  if (mu.supplied) fixed.push_back("mu");
  if (sigma.supplied) fixed.push_back("sigma");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma},{"epsilon",r.epsilon}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
