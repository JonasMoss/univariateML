#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/sn/mle.hpp"
namespace umvl_binding {
inline FitResult fit_sn(cppunivariate::span x, Held xi_arg = {}, Held omega_arg = {}, Held alpha_arg = {}, double reltol=1e-10, int iterlim=100) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(reltol>0 && std::isfinite(reltol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
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
      (alpha.supplied && !std::isfinite(a))) return invalid_argument(fail("Fixed xi and alpha must be finite; fixed omega must be finite and positive."));
  const auto fit=cppunivariate::sn_mle(x,reltol,iterlim,c,w,a);
  const auto o = cppunivariate::outcome(fit);
  const auto done = [&](FitResult out) {
    // This classification uses observations/holds absent from the core result.
    if (out.error_kind == "nomle" && o.reason == cppunivariate::OutcomeReason::InvalidInput)
      return with_outcome(std::move(out), cppunivariate::FitOutcome::nonexistence(cppunivariate::OutcomeReason::Unbounded), static_cast<int>(fit.status));
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(fit.status));
    return with_outcome(std::move(out), o, static_cast<int>(fit.status));
  };

  if(fit.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("sn numerical accuracy failure: requested coefficient accuracy cannot be established."),fit.stop_detail,outcome_kind(o),{"xi","omega","alpha"}));
  if(fit.stop_detail.comparison_tie && std::isfinite(fit.stop_detail.loglik_upper) &&
     fit.stop_detail.loglik_lower==fit.stop_detail.loglik_upper)
    return done(classify(fail("Skew-normal shape is not identifiable when every observation equals the held location."),fit.stop_detail,"tie",{"xi","omega","alpha"}));
  if(fit.stop_detail.comparison_tie)return done(classify(fail("Skew-normal search did not converge or could not certify the maximum; increase iterlim."),fit.stop_detail,"tie",{"xi","omega","alpha"}));
  if(fit.status==cppunivariate::SnStatus::AccuracyFailure)return done(stop_failure(fail("sn numerical accuracy failure: coefficients cannot be represented at the requested accuracy."),fit.stop_detail,outcome_kind(o),{"xi","omega","alpha"}));
  if(fit.status==cppunivariate::SnStatus::RightHalfNormalBoundary)
    return done(classify(fail("The skew-normal MLE is infinite: use `mlhnorm`."),fit.stop_detail,outcome_kind(o),{"mu","sigma"},"hnorm"));
  if(fit.status==cppunivariate::SnStatus::LeftHalfNormalBoundary)
    return done(classify(fail("The skew-normal MLE is infinite: use `mlhnorm` on `-x`."),fit.stop_detail,outcome_kind(o),{"mu","sigma"},"unavailable:reflected-half-normal"));
  if(fit.status==cppunivariate::SnStatus::InvalidInput) return done((x.size()>=2 && !omega.supplied && (!xi.supplied || c==x[0]) && std::isfinite(x[0]) && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) ? classify(fail("Skew-normal fitting requires distinct finite observations, positive reltol and positive iterlim."),{},"nomle",{}) : fail("Skew-normal fitting requires distinct finite observations, positive reltol and positive iterlim."));
  if(fit.budget_exhausted)return done(classify(fail("Skew-normal search did not converge or could not certify the maximum; increase iterlim."),fit.stop_detail,outcome_kind(o),{"xi","omega","alpha"}));
  if(fit.status!=cppunivariate::SnStatus::Ok) return done(classify(fail("Skew-normal search did not converge or could not certify the maximum; increase iterlim."),fit.stop_detail,outcome_kind(o),{"xi","omega","alpha"}));
  std::vector<std::string> fixed;
  if(xi.supplied) fixed.push_back("xi");
  if(omega.supplied) fixed.push_back("omega");
  if(alpha.supplied) fixed.push_back("alpha");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"xi",fit.xi},{"omega",fit.omega},{"alpha",fit.alpha}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
