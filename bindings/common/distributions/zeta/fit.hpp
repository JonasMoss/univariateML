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
    return invalid_argument(fail("Invalid held `s`."));
  const auto fit=cppunivariate::zeta_mle(x,reltol,iterlim,held);
  const auto o=cppunivariate::outcome(fit);
  const auto done=[&](FitResult out) {
    if(!out.error.empty() && out.error_kind!="tie" && o.kind==cppunivariate::OutcomeKind::Estimate)
      return overridden(std::move(out),static_cast<int>(fit.status));
    return with_outcome(std::move(out),o,static_cast<int>(fit.status));
  };
  return done([&]() -> FitResult {
  std::string stop_kind;
  switch(fit.status) {
    case cppunivariate::ZetaStatus::PointMass: stop_kind=outcome_kind(o);break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,fit.stop_detail,stop_kind,{"s"});
  };

  if (fit.status==cppunivariate::ZetaStatus::PointMass)
    return stopped_fail("The maximum likelihood estimator does not exist: all observations are one (point mass at one).");
  if (fit.status==cppunivariate::ZetaStatus::InvalidInput)
    return stopped_fail("Fitting requires positive integer observations, positive finite reltol and positive iterlim.");
  if (fit.status!=cppunivariate::ZetaStatus::Ok)
    return stopped_fail("Maximum likelihood search failed to converge; increase iterlim.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"s",fit.s}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = s.supplied?std::vector<std::string>{"s"}:std::vector<std::string>{}; out.has_fixed = true; return out; }();

  }());
}
}
