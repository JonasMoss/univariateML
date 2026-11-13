#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/slash/mle.hpp"
namespace umvl_binding {
inline FitResult fit_slash(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, double reltol=1e-10, int iterlim=500) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(reltol>0 && std::isfinite(reltol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const double f_sigma=sigma.supplied?sigma.value:cppunivariate::special::kNaN;
  if((sigma.supplied && !(std::isfinite(f_sigma) && f_sigma>0)))return invalid_argument(fail("Fixed scales must be positive finite numbers."));
  const double f_mu=mu.supplied?mu.value:cppunivariate::special::kNaN;
  if(mu.supplied&&!std::isfinite(f_mu))return invalid_argument(fail("Held mu must be finite."));
  const auto f=cppunivariate::slash_mle(x,reltol,iterlim,f_sigma,f_mu);
  const auto o = cppunivariate::outcome(f);
  const auto done = [&](FitResult out) {
    if (f.status == cppunivariate::SlashStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(f.status));
    // Preserve the classifier's refusal of a structured stop (invalid context
    // or missing finite payload); the core result does not retain that context.
    if (!out.error.empty() && out.error_kind.empty() && o.kind != cppunivariate::OutcomeKind::Failure)
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::Evaluation), static_cast<int>(f.status));
    return with_outcome(std::move(out), o, static_cast<int>(f.status));
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };
  if(f.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("slash numerical accuracy failure: requested coefficient accuracy cannot be established."),f.stop_detail,outcome_kind(o),{"mu","sigma"}));
  if(f.accuracy_failure)return done(stop_failure(fail("slash numerical accuracy failure: final Newton refinement did not converge to the requested coefficient tolerance; increase iterlim or inspect conditioning."),f.stop_detail,outcome_kind(o),{"mu","sigma"}));
  if(f.status==cppunivariate::SlashStatus::Unbounded)return done(nomle("Slash likelihood is unbounded: a value has more than half the sample mass."));
  if(f.status==cppunivariate::SlashStatus::CollapseLimit)return done(classify(fail("Slash likelihood selects scale collapse; no finite fit is returned."),{},"nomle",{},""));
  if(f.budget_exhausted)return done(classify(fail("Slash fit failed: check inputs, reltol and iterlim. Raising iterlim may resolve this stop."),f.stop_detail,outcome_kind(o),{"mu","sigma"}));
  if(f.status!=cppunivariate::SlashStatus::Ok)return done(fail("Slash fit failed: check inputs, reltol and iterlim."));
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",f.mu},{"sigma",f.sigma}}; out.logLik = f.logLik; out.iterations = f.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
