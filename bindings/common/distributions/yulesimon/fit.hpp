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
    return invalid_argument(fail("Invalid held `rho`."));
  const auto fit=cppunivariate::yulesimon_mle(x,reltol,iterlim,held);
  const auto o = cppunivariate::outcome(fit);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (fit.status == cppunivariate::YulesimonStatus::Ok)
        return overridden(std::move(out), static_cast<int>(fit.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(fit.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(fit.status));
  };
  std::string stop_kind;
  switch(fit.status) {
    case cppunivariate::YulesimonStatus::PointMass: stop_kind=outcome_kind(o);break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,fit.stop_detail,stop_kind,{"rho"});
  };

  if (fit.status==cppunivariate::YulesimonStatus::PointMass)
    return done(stopped_fail("The maximum likelihood estimator does not exist: all observations are one (point mass at one)."));
  if (fit.status==cppunivariate::YulesimonStatus::InvalidInput)
    return done(stopped_fail("Fitting requires positive integer observations, positive finite reltol and positive iterlim."));
  if (fit.status!=cppunivariate::YulesimonStatus::Ok)
    return done(stopped_fail("Maximum likelihood search failed to converge; increase iterlim."));
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"rho",fit.rho}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = rho.supplied?std::vector<std::string>{"rho"}:std::vector<std::string>{}; out.has_fixed = true; return out; }());

}
}
