#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/foldnorm/mle.hpp"
namespace umvl_binding {
inline FitResult fit_foldnorm(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, double reltol=1e-10, int iterlim=200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const double m=mu.supplied ? mu.value : NAN;
  const double s=sigma.supplied ? sigma.value : NAN;
  if (mu.supplied && !(std::isfinite(m) && m>=0)) return fail("fixed `mu` must be finite and nonnegative.");
  if (sigma.supplied && !(std::isfinite(s) && s>0)) return fail("fixed `sigma` must be finite and positive.");
  const auto r=cppunivariate::foldnorm_mle(x,reltol,iterlim,m,s);
  if (r.status==cppunivariate::FoldnormStatus::Unbounded) return fail("The folded-normal likelihood is unbounded as sigma tends to zero for these constant data.");
  if (r.status==cppunivariate::FoldnormStatus::InvalidInput) return fail("Folded normal requires finite nonnegative data, positive reltol and positive iterlim.");
  if (r.status!=cppunivariate::FoldnormStatus::Ok) return fail("Folded-normal profile search did not converge; increase iterlim.");
  std::vector<std::string> fixed;
  if (mu.supplied) fixed.push_back("mu");
  if (sigma.supplied) fixed.push_back("sigma");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
