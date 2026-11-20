#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/slash/mle.hpp"
namespace umvl_binding {
inline FitResult fit_slash(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, double tol=1e-10, int iterlim=500) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const double f_sigma=sigma.supplied?sigma.value:cppunivariate::special::kNaN;
  if((sigma.supplied && !(std::isfinite(f_sigma) && f_sigma>0)))return invalid_argument(fail("Fixed scales must be positive finite numbers."));
  const double f_mu=mu.supplied?mu.value:cppunivariate::special::kNaN;
  if(mu.supplied&&!std::isfinite(f_mu))return invalid_argument(fail("Held mu must be finite."));
  const auto f=cppunivariate::slash_mle(x,tol,iterlim,f_sigma,f_mu);
  const auto o = cppunivariate::outcome(f);
  const auto done = [&](FitResult out) {
    if (f.status == cppunivariate::SlashStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(f.status));
    return with_outcome(std::move(out), o, static_cast<int>(f.status),{{"mu",mu_arg.supplied},{"sigma",sigma_arg.supplied}});
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };
  if(f.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("slash numerical accuracy failure: requested coefficient accuracy cannot be established."),f.stop_detail,outcome_payload(o),{"mu","sigma"}));
  if(f.accuracy_failure)return done(stop_failure(fail("slash numerical accuracy failure: final Newton refinement did not converge to the requested coefficient tolerance; increase iterlim or inspect conditioning."),f.stop_detail,outcome_payload(o),{"mu","sigma"}));
  if(f.status==cppunivariate::SlashStatus::Unbounded)return done(nomle("Slash likelihood is unbounded: a value has more than half the sample mass."));
  if(f.status==cppunivariate::SlashStatus::CollapseLimit && f.margin_decided)return done(stop_failure(fail("Slash fit unresolved: scale collapse at half multiplicity is within the likelihood margin of the best finite fit; neither the collapse nor a finite maximum is proved."),f.stop_detail,outcome_payload(o),{"mu","sigma"}));
  if(f.status==cppunivariate::SlashStatus::CollapseLimit)return done(stop_failure(fail("Slash likelihood selects scale collapse; no finite fit is returned."),{},PayloadKind::Nonexistence,{},""));
  if(f.budget_exhausted)return done(stop_failure(fail("Slash fit failed: check inputs, tol and iterlim. Raising iterlim may resolve this stop."),f.stop_detail,outcome_payload(o),{"mu","sigma"}));
  if(f.status!=cppunivariate::SlashStatus::Ok)return done(fail("Slash fit failed: check inputs, tol and iterlim."));
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",f.mu},{"sigma",f.sigma}}; out.logLik = f.logLik; out.iterations = f.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
