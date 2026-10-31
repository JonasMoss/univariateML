#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/betapr/mle.hpp"
#include <cmath>
namespace umvl_binding {
inline FitResult fit_betapr(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held scale_arg = {}, double gradtol = 1e-6, int iterlim = 1000, double shape10 = 0, double shape20 = 0, double scale0 = 0, double reltol = 1e-13, double abstol = 1e-10) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& message) { return failure(message, warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held scale = scale_arg;

  const double held = scale.supplied ? scale.value : 1.0;
  if (!(held > 0) || !std::isfinite(held))
    return fail("`scale` must be a positive finite number.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=shape1.supplied?shape1.value:nan;
  const double b=shape2.supplied?shape2.value:nan;
  if((shape1.supplied&&(!(a>0)||!std::isfinite(a))) ||
     (shape2.supplied&&(!(b>0)||!std::isfinite(b))))
    return fail("Held `shape1` and `shape2` must be positive finite numbers.");
  auto res = (shape1.supplied||shape2.supplied)
      ? cppunivariate::betapr_mle_holds(x,a,b,scale.supplied?held:nan,gradtol,iterlim)
      : scale.supplied
      ? cppunivariate::betapr_mle_fixed_scale(x, held, gradtol, iterlim)
      : cppunivariate::betapr_mle(x, gradtol, iterlim,
                                      shape10, shape20, scale0, reltol, abstol);
  if (res.status == cppunivariate::BetaPrimeStatus::GammaBoundary)
    return fail("The beta-prime likelihood selects the gamma limit. Use `mlgamma` to fit this model.");
  if (res.status == cppunivariate::BetaPrimeStatus::InvGammaBoundary)
    return fail("The beta-prime likelihood selects the inverse-gamma limit. Use `mlinvgamma` to fit this model.");
  if (res.status == cppunivariate::BetaPrimeStatus::BfgsFailure)
    return fail("Beta-prime optimizer BfgsFailure: profile did not converge or exceeded its numerical search range; increase iterlim.");
  if (res.status == cppunivariate::BetaPrimeStatus::InvalidStart ||
      !std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.scale))
    return fail("Beta-prime MLE requires positive finite observations with positive log-variance.");
  std::vector<std::string> fixed;
  if(shape1.supplied)fixed.push_back("shape1");
  if(shape2.supplied)fixed.push_back("shape2");
  if(scale.supplied)fixed.push_back("scale");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.alpha},{"shape2",res.beta},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
