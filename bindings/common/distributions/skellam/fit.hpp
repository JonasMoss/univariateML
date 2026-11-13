#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/skellam/mle.hpp"
namespace umvl_binding {
inline FitResult fit_skellam(cppunivariate::span x, Held mu1_arg = {}, Held mu2_arg = {}, double reltol=1e-10, int iterlim=150) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu1 = mu1_arg;
  const Held mu2 = mu2_arg;

  const double a=mu1.supplied?mu1.value:NAN;
  const double b=mu2.supplied?mu2.value:NAN;
  if((mu1.supplied && !(std::isfinite(a) && a>=0)) ||
     (mu2.supplied && !(std::isfinite(b) && b>=0))) return invalid_argument(fail("Fixed rates must be finite and nonnegative."));
  const auto fit=cppunivariate::skellam_mle(x,reltol,iterlim,a,b);
  const auto o = cppunivariate::outcome(fit);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (fit.status == cppunivariate::SkellamStatus::Ok)
        return overridden(std::move(out), static_cast<int>(fit.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(fit.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(fit.status));
  };
  if(fit.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("skellam numerical accuracy failure: requested coefficient accuracy cannot be established."),fit.stop_detail,outcome_kind(o),{"mu1","mu2"}));
  if(fit.status==cppunivariate::SkellamStatus::InvalidInput) return done(fail("Skellam requires finite integer observations, positive reltol and positive iterlim."));
  if(fit.status==cppunivariate::SkellamStatus::Impossible) return done(fail("The fixed rates give zero likelihood for these observations."));
  if(fit.stop_detail.budget_exhausted)return done(stop_failure(fail("Skellam search did not converge; increase iterlim. Raising iterlim may resolve this stop."),fit.stop_detail,outcome_kind(o),{"mu1","mu2"}));
  if(fit.status!=cppunivariate::SkellamStatus::Ok) return done(fail("Skellam search did not converge; increase iterlim."));
  std::vector<std::string> fixed;
  if(mu1.supplied) fixed.push_back("mu1");
  if(mu2.supplied) fixed.push_back("mu2");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu1",fit.mu1},{"mu2",fit.mu2}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
