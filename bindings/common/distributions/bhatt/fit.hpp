#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/bhatt/mle.hpp"
namespace umvl_binding {
inline FitResult fit_bhatt(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held a_arg = {}, double reltol=1e-8, int iterlim=500) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& message) { return failure(message, warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;
  const Held a = a_arg;

  const double f_sigma=sigma.supplied?sigma.value:cppunivariate::special::kNaN;
  const double f_a=a.supplied?a.value:cppunivariate::special::kNaN;
  if((sigma.supplied && !(std::isfinite(f_sigma) && f_sigma>0)) || (a.supplied && !(std::isfinite(f_a) && f_a>=0)))return fail("Fixed sigma must be positive and a nonnegative; both must be finite.");
  const double f_mu=mu.supplied?mu.value:cppunivariate::special::kNaN;
  if(mu.supplied&&!std::isfinite(f_mu))return fail("Held mu must be finite.");
  const auto f=cppunivariate::bhatt_mle(x,reltol,iterlim,f_sigma,f_a,f_mu);
  if(f.status==cppunivariate::BhattStatus::Unbounded)return fail("Bhattacharjee likelihood is unbounded for a constant sample.");
  if(f.status==cppunivariate::BhattStatus::NormalLimit) {
    if(sigma.supplied)return fail("Bhattacharjee likelihood selects the normal face with sigma held. Use `mlnorm` for an unrestricted fit; it does not hold sigma.");
    return fail("Bhattacharjee likelihood selects the normal face: use `mlnorm`.");
  }
  if(f.status==cppunivariate::BhattStatus::UniformLimit) {
    if(a.supplied)return fail("Bhattacharjee likelihood selects the uniform face with a held. Use `mlunif` for an unrestricted fit; it does not hold a.");
    return fail("Bhattacharjee likelihood selects the uniform face: use `mlunif`.");
  }
  if(f.status!=cppunivariate::BhattStatus::Ok)return fail("Bhatt fit failed: check inputs, reltol and iterlim.");
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");
  if(a.supplied)fixed.push_back("a");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",f.mu},{"sigma",f.sigma},{"a",f.a}}; out.logLik = f.logLik; out.iterations = f.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
