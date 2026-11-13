#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gev/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_gev(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held xi_arg = {}, double reltol = 1e-10, int iterlim = 100) {
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
  const Held xi = xi_arg;

  const double shape=xi.supplied ? xi.value : std::numeric_limits<double>::quiet_NaN();
  if(xi.supplied && (!std::isfinite(shape) || (shape < -1 && !mu.supplied)))
    return invalid_argument(fail("`xi` must be finite and in [-1, 1]."));
  const double location=mu.supplied ? mu.value : std::numeric_limits<double>::quiet_NaN();
  const double scale=sigma.supplied ? sigma.value : std::numeric_limits<double>::quiet_NaN();
  if(mu.supplied && !std::isfinite(location)) return invalid_argument(fail("`mu` must be finite."));
  if(sigma.supplied && (!(scale>0) || !std::isfinite(scale))) return invalid_argument(fail("`sigma` must be positive and finite."));
  std::vector<std::string> fixed;
  if(mu.supplied) fixed.push_back("mu");
  if(sigma.supplied) fixed.push_back("sigma");
  if(xi.supplied) fixed.push_back("xi");
  const auto r=cppunivariate::gev_mle(x,reltol,iterlim,shape,location,scale);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    if (r.status == cppunivariate::GevStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(r.status));
    // The adapter's sample/hold checks distinguish collapse from invalid input.
    if (out.error_kind == "nomle" && o.kind != cppunivariate::OutcomeKind::Nonexistence)
      return with_outcome(std::move(out), cppunivariate::FitOutcome::nonexistence(cppunivariate::OutcomeReason::Unbounded), static_cast<int>(r.status));
    // Preserve the classifier's refusal of a structured stop (invalid context
    // or missing finite payload); the core result does not retain that context.
    if (!out.error.empty() && out.error_kind.empty() && o.kind != cppunivariate::OutcomeKind::Failure)
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::Evaluation), static_cast<int>(r.status));
    return with_outcome(std::move(out), o, static_cast<int>(r.status));
  };
  if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("gev numerical accuracy failure: requested coefficient accuracy cannot be established."),r.stop_detail,outcome_kind(o),{"mu","sigma","xi"}));
  if(r.status==cppunivariate::GevStatus::SearchContradicted)
    return done(stop_failure(fail("Generalized extreme value search is disproved by a certified better feasible point."),r.stop_detail,outcome_kind(o),{"mu","sigma","xi"}));
  if(r.stop_detail.budget_exhausted)
    return done(stop_failure(fail("Generalized extreme value certificate exhausted its work budget; raise iterlim."),r.stop_detail,outcome_kind(o),{"mu","sigma","xi"}));
  if(r.stop_detail.comparison_tie)
    return done(stop_failure(fail("Generalized extreme value certificate cannot separate competing shape maxima within its floating-point margin."),r.stop_detail,"tie",{"mu","sigma","xi"}));
  if(r.status==cppunivariate::GevStatus::NumericalFailure ||
     (r.status==cppunivariate::GevStatus::IterationFailure && !std::isnan(r.stop_detail.loglik_upper)))
    return done(stop_failure(fail("Generalized extreme value certificate failed to converge or resolve the maximum numerically; raise iterlim for an exhausted solve."),r.stop_detail,outcome_kind(o),{"mu","sigma","xi"}));
  if(r.status==cppunivariate::GevStatus::NoMLE)
    return done(classify(fail("Generalized extreme value has no finite maximum: minimum multiplicity permits scale collapse."),r.stop_detail,outcome_kind(o),{"mu","sigma","xi"}));
  const bool valid_sample=x.size()>=3 && reltol>0 && std::isfinite(reltol) && iterlim>0 &&
    std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);});
  bool minimum_collapse=false;
  if(valid_sample && !mu.supplied && !sigma.supplied) {
    const double minimum=*std::min_element(x.begin(),x.end());
    const double k=static_cast<double>(std::count(x.begin(),x.end(),minimum));
    minimum_collapse=std::isnan(shape)?2*k>x.size():shape>0 && shape*k>=x.size()-k;
  }
  if(r.status==cppunivariate::GevStatus::InvalidData)
    return done((minimum_collapse || (x.size()>=3 && std::isfinite(x[0]) && !sigma.supplied && (!mu.supplied || location==x[0]) && reltol>0 && std::isfinite(reltol) && iterlim>0 && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))) ? classify(fail("Generalized extreme value requires at least three finite observations, an admissible multiplicity at the minimum, finite range, compatible support and holds, positive finite `reltol` and positive `iterlim`."),{},"nomle",{}) : fail("Generalized extreme value requires at least three finite observations, an admissible multiplicity at the minimum, finite range, compatible support and holds, positive finite `reltol` and positive `iterlim`."));
  if(r.status!=cppunivariate::GevStatus::Ok || !std::isfinite(r.logLik))
    return done(fail("Generalized extreme value MLE failed to converge."));
  std::vector<std::pair<std::string,std::string>> restricted;
  if(r.restricted & 1u) restricted.emplace_back("mu","lower");
  if(r.restricted & 2u) restricted.emplace_back("mu","upper");
  if(r.restricted & 4u) restricted.emplace_back("sigma","lower");
  if(r.restricted & 8u) restricted.emplace_back("sigma","upper");
  if(r.restricted & 16u) restricted.emplace_back("xi","lower");
  if(r.restricted & 32u) restricted.emplace_back("xi","upper");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma},{"xi",r.xi}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; if(!xi.supplied && (!mu.supplied || !sigma.supplied))out.extra.emplace_back("certified",r.certified?1.:0.); return out; }());

}
}
