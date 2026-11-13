#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/emg/mle.hpp"
namespace umvl_binding {
inline FitResult fit_emg(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held lambda_arg = {}, double reltol=1e-10, int iterlim=500) {
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
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;
  const Held lambda = lambda_arg;

    double checked_value_0 = std::numeric_limits<double>::quiet_NaN();
  if (mu.supplied) {
const Held a = mu;

    const double v=a.value;
    if(!std::isfinite(v))return invalid_argument(fail("Held parameters must be finite."));
    checked_value_0 = v;

  }
  double checked_value_1 = std::numeric_limits<double>::quiet_NaN();
  if (sigma.supplied) {
const Held a = sigma;

    const double v=a.value;
    if(!std::isfinite(v))return invalid_argument(fail("Held parameters must be finite."));
    checked_value_1 = v;

  }
  double checked_value_2 = std::numeric_limits<double>::quiet_NaN();
  if (lambda.supplied) {
const Held a = lambda;

    const double v=a.value;
    if(!std::isfinite(v))return invalid_argument(fail("Held parameters must be finite."));
    checked_value_2 = v;

  }

  auto r=cppunivariate::emg_mle(x,reltol,iterlim,checked_value_0,checked_value_1,checked_value_2);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (r.status == cppunivariate::EmgStatus::Ok)
        return overridden(std::move(out), static_cast<int>(r.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(r.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(r.status));
  };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };
  if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("emg numerical accuracy failure: requested coefficient accuracy cannot be established."),r.stop_detail,outcome_kind(o),{"mu","sigma","lambda"}));
  if(r.status==cppunivariate::EmgStatus::AccuracyFailure)return done(stop_failure(fail("emg numerical accuracy failure: coefficients cannot be represented at the requested accuracy."),r.stop_detail,outcome_kind(o),{"mu","sigma","lambda"}));
  if(r.status==cppunivariate::EmgStatus::BudgetExhausted)return done(stop_failure(fail("EMG certificate budget exhausted; raising iterlim may resolve the remaining shape intervals."),r.stop_detail,outcome_kind(o),{"mu","sigma","lambda"}));
  if(r.status==cppunivariate::EmgStatus::Unresolved)return done(stop_failure(fail("EMG normal face unresolved: no verified finite winner beats it; use mlnorm only for the compared normal model, retaining its holds."),r.stop_detail,outcome_kind(o),{"mu","sigma","lambda"}));
  if(r.status==cppunivariate::EmgStatus::Tie)return done(stop_failure(fail("EMG separated surviving maximizers or face comparison could not be separated."),r.stop_detail,"tie",{"mu","sigma","lambda"}));
  if(r.status==cppunivariate::EmgStatus::NumericalFailure)return done(classify(fail("EMG numerical accuracy failure: score enclosure, shape bound or arithmetic could not be resolved."),r.stop_detail,outcome_kind(o),{"mu","sigma","lambda"}));
  if(r.status==cppunivariate::EmgStatus::IterationFailure && !mu.supplied && sigma.supplied && lambda.supplied)return done(classify(fail("EMG location solve did not converge; increase iterlim."),r.stop_detail,outcome_kind(o),{"mu","sigma","lambda"}));
  if(r.stop_detail.comparison_tie)return done(classify(fail("EMG search did not converge; increase iterlim."),r.stop_detail,"tie",{"mu","sigma","lambda"}));
  if(r.status==cppunivariate::EmgStatus::NormalLimit)return done(classify(fail("The EMG MLE does not exist: normal face; use mlnorm for an unrestricted normal fit. The compared face retains held mu/sigma."),r.stop_detail,outcome_kind(o),{"mean","sd"},"norm"));
  if(r.status==cppunivariate::EmgStatus::TranslatedExponentialLimit)return done(classify(fail("The EMG MLE does not exist: translated exponential face. No translated exponential fitter is available; for a free endpoint fit distances with mlexp(x - min(x))."),r.stop_detail,outcome_kind(o),{"mu","rate"},"unavailable:translated-exponential"));
  if(r.status==cppunivariate::EmgStatus::Unbounded)return done(nomle("The EMG likelihood is unbounded for these constant observations."));
  if(r.status==cppunivariate::EmgStatus::InvalidInput)return done(fail("EMG requires finite data, finite mu, positive sigma and lambda, positive reltol and iterlim."));
  if(r.status==cppunivariate::EmgStatus::IterationFailure)return done(stop_failure(fail("EMG certificate inner solve did not converge; increase iterlim."),r.stop_detail,outcome_kind(o),{"mu","sigma","lambda"}));
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");
  if(lambda.supplied)fixed.push_back("lambda");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma},{"lambda",r.lambda}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
