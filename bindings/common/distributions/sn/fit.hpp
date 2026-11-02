#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/sn/mle.hpp"
namespace umvl_binding {
inline FitResult fit_sn(cppunivariate::span x, Held xi_arg = {}, Held omega_arg = {}, Held alpha_arg = {}, double reltol=1e-9, int iterlim=100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held xi = xi_arg;
  const Held omega = omega_arg;
  const Held alpha = alpha_arg;

  const double nan=cppunivariate::special::kNaN;
  const double c=xi.supplied ? xi.value : nan;
  const double w=omega.supplied ? omega.value : nan;
  const double a=alpha.supplied ? alpha.value : nan;
  if ((xi.supplied && !std::isfinite(c)) ||
      (omega.supplied && !(std::isfinite(w) && w>0)) ||
      (alpha.supplied && !std::isfinite(a))) return fail("Fixed xi and alpha must be finite; fixed omega must be finite and positive.");
  const auto fit=cppunivariate::sn_mle(x,reltol,iterlim,c,w,a);
  if(fit.status==cppunivariate::SnStatus::RightHalfNormalBoundary)
    return fail("The skew-normal MLE is infinite: use `mlhnorm`.");
  if(fit.status==cppunivariate::SnStatus::LeftHalfNormalBoundary)
    return fail("The skew-normal MLE is infinite: use `mlhnorm` on `-x`.");
  if(fit.status==cppunivariate::SnStatus::InvalidInput) return fail("Skew-normal fitting requires distinct finite observations, positive reltol and positive iterlim.");
  if(fit.status!=cppunivariate::SnStatus::Ok) return fail("Skew-normal search did not converge or could not certify the maximum; increase iterlim.");
  std::vector<std::string> fixed;
  if(xi.supplied) fixed.push_back("xi");
  if(omega.supplied) fixed.push_back("omega");
  if(alpha.supplied) fixed.push_back("alpha");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"xi",fit.xi},{"omega",fit.omega},{"alpha",fit.alpha}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
