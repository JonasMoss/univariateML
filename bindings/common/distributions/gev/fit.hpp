#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gev/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_gev(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held xi_arg = {}, double reltol = 1e-10, int iterlim = 100) {

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
    if (r.status == cppunivariate::GevStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(r.status));
    return with_outcome(std::move(out), o, static_cast<int>(r.status),{{"mu",mu_arg.supplied},{"sigma",sigma_arg.supplied},{"xi",xi_arg.supplied}});
  };
  if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("gev numerical accuracy failure: requested coefficient accuracy cannot be established."),r.stop_detail,outcome_payload(o),{"mu","sigma","xi"}));
  if(r.status==cppunivariate::GevStatus::SearchContradicted)
    return done(stop_failure(fail("Generalized extreme value search is disproved by a certified better feasible point."),r.stop_detail,outcome_payload(o),{"mu","sigma","xi"}));
  if(r.stop_detail.budget_exhausted)
    return done(stop_failure(fail("Generalized extreme value certificate exhausted its work budget; raise iterlim."),r.stop_detail,outcome_payload(o),{"mu","sigma","xi"}));
  if(r.stop_detail.comparison_tie)
    return done(comparison_payload(stop_failure(fail("Generalized extreme value certificate cannot separate competing shape maxima within its floating-point margin."),r.stop_detail,PayloadKind::Comparison,{"mu","sigma","xi"}),o,cppunivariate::comparison_choice(r),{{"mu",mu_arg},{"sigma",sigma_arg},{"xi",xi_arg}}));
  if(r.status==cppunivariate::GevStatus::NumericalFailure ||
     (r.status==cppunivariate::GevStatus::IterationFailure && !std::isnan(r.stop_detail.loglik_upper)))
    return done(stop_failure(fail("Generalized extreme value certificate failed to converge or resolve the maximum numerically; raise iterlim for an exhausted solve."),r.stop_detail,outcome_payload(o),{"mu","sigma","xi"}));
  if(r.status==cppunivariate::GevStatus::NoMLE)
    return done(stop_failure(fail("Generalized extreme value has no finite maximum: minimum multiplicity permits scale collapse."),r.stop_detail,outcome_payload(o),{"mu","sigma","xi"}));
  if(r.status==cppunivariate::GevStatus::InvalidData)
    return done(fail("Generalized extreme value requires finite observations, compatible support and holds, positive finite `reltol` and positive `iterlim`."));
  if(r.status!=cppunivariate::GevStatus::Ok || !std::isfinite(r.logLik))
    return done(fail("Generalized extreme value MLE failed to converge."));
  std::vector<std::pair<std::string,std::string>> restricted;
  if(r.restricted & 1u) restricted.emplace_back("mu","lower");
  if(r.restricted & 2u) restricted.emplace_back("mu","upper");
  if(r.restricted & 4u) restricted.emplace_back("sigma","lower");
  if(r.restricted & 8u) restricted.emplace_back("sigma","upper");
  if(r.restricted & 16u) restricted.emplace_back("xi","lower");
  if(r.restricted & 32u) restricted.emplace_back("xi","upper");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma},{"xi",r.xi}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true;  return out; }());

}
}
