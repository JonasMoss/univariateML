#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/cauchy/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_cauchy(cppunivariate::span x, Held location_arg = {}, Held scale_arg = {}, double tol = 1e-10, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held location = location_arg;
  const Held scale = scale_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_location=location.supplied?location.value:nan;
  if(location.supplied && (!std::isfinite(held_location)))return invalid_argument(fail("`location` must be finite."));
  const double held_scale=scale.supplied?scale.value:nan;
  if(scale.supplied && (!std::isfinite(held_scale) || !(held_scale>0)))return invalid_argument(fail("`scale` must be positive and finite."));
  std::vector<std::string> fixed;
  if(location.supplied)fixed.push_back("location");
  if(scale.supplied)fixed.push_back("scale");

  cppunivariate::CauchyResult res = cppunivariate::fit_cauchy(x,tol,iterlim,held_location,held_scale);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if(!res.maximizer_support.empty())out.extra_vectors.push_back({"maximizer_support",res.maximizer_support});
    if (res.status == cppunivariate::CauchyStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    if (o.kind == cppunivariate::OutcomeKind::Estimate)
      attach_maximizer_set(out, res.maximizers, "cauchy", [&](const auto& coefficients) {
        double ll = 0;
        for (double observation : x)
          ll += cppunivariate::distributions::dcauchy(observation, coefficients[0], coefficients[1], true);
        return ll;
      });
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"location",location_arg.supplied},{"scale",scale_arg.supplied}});
  };
  if(res.status==cppunivariate::CauchyStatus::NumericalFailure)return done(fail("Cauchy likelihood cannot be evaluated at the represented coefficients."));
  if(res.stop_detail.budget_exhausted)return done(stop_failure(failure("Conditional fit did not converge: numerical certificate work budget exhausted. Raising iterlim may resolve this stop."),res.stop_detail,outcome_payload(o),{"location","scale"}));
  if((res.status==cppunivariate::CauchyStatus::NoMLE || res.status==cppunivariate::CauchyStatus::Unattained) && location.supplied)
    return done(stop_failure(fail("With location held the Cauchy scale MLE does not exist when at least half the observations equal that location."),res.stop_detail,outcome_payload(o),{"location","scale"},""));
  if (res.status == cppunivariate::CauchyStatus::NoMLE || res.status==cppunivariate::CauchyStatus::Unattained)
    return done(stop_failure(fail("Cauchy MLE does not exist: a sample atom has at least half the mass, except for balanced two-point samples"),res.stop_detail,outcome_payload(o),{"location","scale"},""));
  if(res.status==cppunivariate::CauchyStatus::AmbiguousMaximum) {
    std::string message="Held-scale Cauchy global maxima are tied or numerically indistinguishable.";
    if(o.kind==cppunivariate::OutcomeKind::Estimate && o.uniqueness_known && !o.unique)
      message+=" The two reflected locations are global maximizers.";
    auto out=comparison_payload(stop_failure(fail(message),res.stop_detail,PayloadKind::Comparison,{"location","scale"},""),
      o,res.selected_candidate,{{"location",location_arg},{"scale",scale_arg}});
    if(o.kind==cppunivariate::OutcomeKind::Estimate) {
      out.logLik=res.candidate_evaluations[o.uniqueness_known && !o.unique ? 0 : res.selected_candidate].loglik;
    }
    return done(out);
  }
  if(res.status==cppunivariate::CauchyStatus::InvalidInput)return done(fail("Cauchy fit requires finite data, positive tolerance and iterlim, and valid held coefficients."));
  if(res.status==cppunivariate::CauchyStatus::IterationFailure)return done(fail("Conditional fit did not converge: numerical certificate, scale bracket or iteration failure; increase iterlim only for iteration exhaustion."));
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    return done(fail("Cauchy MLE failed: non-finite or invalid parameters"));
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"location",res.mu},{"scale",res.scale}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }());

}
}
