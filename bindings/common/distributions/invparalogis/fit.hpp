#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/invparalogis/mle.hpp"
#include <cmath>
#include <algorithm>
#include <limits>
namespace umvl_binding {
inline FitResult fit_invparalogis(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, double tol = 1e-10, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape = shape_arg;
  const Held scale = scale_arg;

 if(shape.supplied && (!(shape.value>0)||!std::isfinite(shape.value))) return invalid_argument(fail("`shape` must be positive and finite."));

  const double held = scale.supplied ? scale.value : 1.0;
  if (!(held > 0) || !std::isfinite(held))
    return invalid_argument(fail("`scale` must be a positive finite number."));
  auto res = shape.supplied ? cppunivariate::invparalogis_mle_holds(x,shape.value,scale.supplied?held:NAN,tol,iterlim) : scale.supplied
      ? cppunivariate::invparalogis_mle_fixed_scale(x, held, tol, iterlim)
      : cppunivariate::invparalogis_mle(x, tol, iterlim);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"shape",shape_arg.supplied},{"scale",scale_arg.supplied}});
  };

  if(res.unrepresentable_search)
    return done(stop_failure(fail("Inverse paralogistic MLE failed: fitted parameters or likelihood exceed double precision"),res.stop_detail,outcome_payload(o),{"shape","scale"}));
  if(res.status==cppunivariate::ParalogisStatus::NumericalFailure && std::isinf(res.scale))
    return done(stop_failure(fail("Inverse paralogistic numerical failure: the fitted scale cannot be represented in binary64."),res.stop_detail,outcome_payload(o),{"shape","scale"}));
  if(res.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("invparalogis numerical accuracy failure: requested coefficient accuracy cannot be established."),res.stop_detail,outcome_payload(o),{"shape","scale"}));
  if(res.status==cppunivariate::ParalogisStatus::Unbounded)
    return done(stop_failure(fail("The invparalogis maximum likelihood estimator does not exist for constant data: the likelihood is unbounded."),res.stop_detail,outcome_payload(o),{"shape","scale"}));
  if(res.status==cppunivariate::ParalogisStatus::CertificateBudget)
    return done(stop_failure(fail("Inverse paralogistic certificate budget exhausted; no verified MLE. Raising iterlim may resolve this stop."),res.stop_detail,outcome_payload(o),{"shape","scale"}));
  if(res.status==cppunivariate::ParalogisStatus::NumericalFailure)
    return done(stop_failure(fail("Inverse paralogistic certificate numerical failure: unresolved bounds or fitted parameters exceed double precision; no verified MLE."),res.stop_detail,outcome_payload(o),{"shape","scale"}));
  if(res.status==cppunivariate::ParalogisStatus::InvalidStart)
    return done(fail("Inverse paralogistic MLE requires positive, finite observations."));
  if(res.status==cppunivariate::ParalogisStatus::BfgsFailure)
    return done(stop_failure(fail("Inverse paralogistic optimizer BfgsFailure: proposal failed to converge; no verified MLE; increase iterlim."),res.stop_detail,outcome_payload(o),{"shape","scale"}));
  std::vector<std::string> fixed;
  if(shape.supplied) fixed.push_back("shape");
  if(scale.supplied) fixed.push_back("scale");
  return done([&]() { FitResult out;  out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
