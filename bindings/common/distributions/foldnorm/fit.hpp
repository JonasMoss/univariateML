#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/foldnorm/mle.hpp"
namespace umvl_binding {
inline FitResult fit_foldnorm(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, double reltol=1e-10, int iterlim=200) {
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

  const double m=mu.supplied ? mu.value : NAN;
  const double s=sigma.supplied ? sigma.value : NAN;
  if (mu.supplied && !(std::isfinite(m) && m>=0)) return invalid_argument(fail("fixed `mu` must be finite and nonnegative."));
  if (sigma.supplied && !(std::isfinite(s) && s>0)) return invalid_argument(fail("fixed `sigma` must be finite and positive."));
  const auto r=cppunivariate::foldnorm_mle(x,reltol,iterlim,m,s);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (r.status == cppunivariate::FoldnormStatus::Ok)
        return overridden(std::move(out), static_cast<int>(r.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(r.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(r.status));
  };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };
  if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("foldnorm numerical accuracy failure: requested coefficient accuracy cannot be established."),r.stop_detail,outcome_kind(o),{"mu","sigma"}));
  if(r.stop_detail.budget_exhausted)return done(stop_failure(failure("Folded-normal profile search did not converge; certificate work budget exhausted. Raising iterlim may resolve this stop."),r.stop_detail,outcome_kind(o),{"mu","sigma"}));
  if (r.status==cppunivariate::FoldnormStatus::AccuracyFailure)
    return done(stop_failure(fail("Foldnorm could not verify the requested coefficient accuracy in binary64."),r.stop_detail,outcome_kind(o),{"mu","sigma"}));
  if (r.status==cppunivariate::FoldnormStatus::Unbounded) return done(nomle("The folded-normal likelihood is unbounded as sigma tends to zero for these constant data."));
  if (r.status==cppunivariate::FoldnormStatus::InvalidInput) return done(fail("Folded normal requires finite nonnegative data, positive reltol and positive iterlim."));
  if (r.status!=cppunivariate::FoldnormStatus::Ok) return done(fail("Folded-normal profile search did not converge; increase iterlim."));
  std::vector<std::string> fixed;
  if (mu.supplied) fixed.push_back("mu");
  if (sigma.supplied) fixed.push_back("sigma");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
