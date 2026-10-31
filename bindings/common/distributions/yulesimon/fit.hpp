#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/yulesimon/mle.hpp"
namespace umvl_binding {
inline FitResult fit_yulesimon(cppunivariate::ispan x, Held rho_arg = {}, double reltol=1e-10, int iterlim=200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held rho = rho_arg;

  const double held=!rho.supplied?std::numeric_limits<double>::quiet_NaN():rho.value;
  if (rho.supplied && (!std::isfinite(held) || !(held>0)))
    return fail("Invalid held `rho`.");
  const auto fit=cppunivariate::yulesimon_mle(x,reltol,iterlim,held);
  if (fit.status==cppunivariate::YulesimonStatus::PointMass)
    return fail("The maximum likelihood estimator does not exist: all observations are one (point mass at one).");
  if (fit.status==cppunivariate::YulesimonStatus::InvalidInput)
    return fail("Fitting requires positive integer observations, positive finite reltol and positive iterlim.");
  if (fit.status!=cppunivariate::YulesimonStatus::Ok)
    return fail("Maximum likelihood search failed to converge; increase iterlim.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"rho",fit.rho}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = rho.supplied?std::vector<std::string>{"rho"}:std::vector<std::string>{}; out.has_fixed = true; return out; }();

}
}
