#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/slash/mle.hpp"
namespace umvl_binding {
inline FitResult fit_slash(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, double reltol=1e-8, int iterlim=500) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const double f_sigma=sigma.supplied?sigma.value:cppunivariate::special::kNaN;
  if((sigma.supplied && !(std::isfinite(f_sigma) && f_sigma>0)))return fail("Fixed scales must be positive finite numbers.");
  const double f_mu=mu.supplied?mu.value:cppunivariate::special::kNaN;
  if(mu.supplied&&!std::isfinite(f_mu))return fail("Held mu must be finite.");
  const auto f=cppunivariate::slash_mle(x,reltol,iterlim,f_sigma,f_mu);
  if(f.status==cppunivariate::SlashStatus::Unbounded)return fail("Slash likelihood is unbounded: a value has more than half the sample mass.");
  if(f.status==cppunivariate::SlashStatus::CollapseLimit)return fail("Slash likelihood selects scale collapse; no finite fit is returned.");
  if(f.status!=cppunivariate::SlashStatus::Ok)return fail("Slash fit failed: check inputs, reltol and iterlim.");
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",f.mu},{"sigma",f.sigma}}; out.logLik = f.logLik; out.iterations = f.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
