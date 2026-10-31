#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/zeta/mle.hpp"
namespace umvl_binding {
inline FitResult fit_zeta(cppunivariate::ispan x, Held s_arg = {}, double reltol=1e-10, int iterlim=200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held s = s_arg;

  const double held=!s.supplied?std::numeric_limits<double>::quiet_NaN():s.value;
  if (s.supplied && (!std::isfinite(held) || !(held>1)))
    return fail("Invalid held `s`.");
  const auto fit=cppunivariate::zeta_mle(x,reltol,iterlim,held);
  if (fit.status==cppunivariate::ZetaStatus::PointMass)
    return fail("The maximum likelihood estimator does not exist: all observations are one (point mass at one).");
  if (fit.status==cppunivariate::ZetaStatus::InvalidInput)
    return fail("Fitting requires positive integer observations, positive finite reltol and positive iterlim.");
  if (fit.status!=cppunivariate::ZetaStatus::Ok)
    return fail("Maximum likelihood search failed to converge; increase iterlim.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"s",fit.s}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = s.supplied?std::vector<std::string>{"s"}:std::vector<std::string>{}; out.has_fixed = true; return out; }();

}
}
