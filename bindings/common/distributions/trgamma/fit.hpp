#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/trgamma/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_trgamma(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held scale_arg = {}, double reltol = 1e-10, int iterlim = 100) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held scale = scale_arg;

 if(scale.supplied && (!(scale.value>0)||!std::isfinite(scale.value))) return invalid_argument(fail("`scale` must be positive and finite."));

  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double a = shape1.supplied ? shape1.value : nan;
  const double b = shape2.supplied ? shape2.value : nan;
  if ((shape1.supplied && (!(a > 0.0) || !std::isfinite(a))) ||
      (shape2.supplied && (!(b > 0.0) || !std::isfinite(b))))
    return invalid_argument(fail("`shape1` and `shape2` must be positive finite numbers when supplied."));
  const cppunivariate::span xs = x;
  auto res = scale.supplied ? cppunivariate::trgamma_mle_fixed_scale(xs,scale.value,a,b,reltol,iterlim) : cppunivariate::trgamma_mle(xs, reltol, iterlim, a, b);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"shape1",shape1_arg.supplied},{"shape2",shape2_arg.supplied},{"scale",scale_arg.supplied}});
  };

  if(res.stop_detail.budget_exhausted)return done(stop_failure(fail("Constrained likelihood search failed to converge. Raising iterlim may resolve this stop."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"}));
  if(res.stop_detail.comparison_tie) {
    if(scale.supplied)return done(comparison_payload(stop_failure(fail("The held-scale interior maximum is too close to the fixed-endpoint power face to certify its coefficients."),res.stop_detail,PayloadKind::Comparison,{"shape1","shape2","scale"}),o,cppunivariate::comparison_choice(res),{{"shape1",shape1_arg},{"shape2",shape2_arg},{"scale",scale_arg}}));
    return done(comparison_payload(stop_failure(fail(res.stop_detail.limit_model==cppunivariate::StopModel::Lnorm ?
      "The interior and lognormal likelihoods cannot be distinguished at the comparison tolerance. Use `mllnorm` for the limiting fit." :
      "The interior and endpoint likelihoods cannot be distinguished at the comparison tolerance. Use `mlpower` for the limiting fit."),res.stop_detail,PayloadKind::Comparison,{"shape1","shape2","scale"}),o,cppunivariate::comparison_choice(res),{{"shape1",shape1_arg},{"shape2",shape2_arg},{"scale",scale_arg}}));
  }
  if(res.transformation_failure)return done(stop_failure(fail("Distinct observations or the held endpoint became indistinguishable after taking logarithms."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"}));
  if(res.reconstruction_failure)return done(stop_failure(fail("Cannot certify the likelihood after reconstructing the coefficients in double precision."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"}));
  if (o.kind == cppunivariate::OutcomeKind::Nonexistence)
    return done(stop_failure(fail("trgamma requires at least two distinct positive finite observations, positive `reltol` and positive `iterlim`; constant samples can have unbounded likelihood."),{},outcome_payload(o),{}));
  if (res.status == cppunivariate::TrGammaStatus::InvalidData)
    return done(fail("trgamma requires at least two distinct positive finite observations, positive `reltol` and positive `iterlim`; constant samples can have unbounded likelihood."));
  if (res.status == cppunivariate::TrGammaStatus::LognormalBoundary)
    return done(stop_failure(fail("The trgamma likelihood is maximised at a limit. Use `mllnorm` to fit this model."),res.stop_detail,outcome_payload(o),{"meanlog","sdlog"},"lnorm"));
  if (res.status == cppunivariate::TrGammaStatus::PowerBoundary && scale.supplied)
    return done(stop_failure(fail("The held-scale likelihood selects the limiting fixed-endpoint power face at scale; no matching held-endpoint fitter is available."),res.stop_detail,outcome_payload(o),{"alpha","beta"},"power"));
  if (res.status == cppunivariate::TrGammaStatus::PowerBoundary)
    return done(stop_failure(fail("The trgamma likelihood is maximised at a limit. Use `mlpower` to fit this model."),res.stop_detail,outcome_payload(o),{"alpha","beta"},"power"));
  if (res.status == cppunivariate::TrGammaStatus::ScaleOutOfRange) {
    const double gap = res.logLik - cppunivariate::lognormal_mle(xs).logLik;
    auto detail=res.stop_detail;detail.loglik_upper=INFINITY;
    return done(stop_failure(fail("The trgamma maximum likelihood fit lies so close to the lognormal limit "
               "that `scale` is not representable with normal double precision (log(scale) = %g). "
               "Use `mllnorm`: its log-likelihood is lower by only %g.",
               res.log_scale, gap),detail,outcome_payload(o),{"shape1","shape2","scale"}));
  }
  if (res.status == cppunivariate::TrGammaStatus::IterationFailure) return done(stop_failure(fail("Constrained likelihood search failed to converge."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"}));
  if (!std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale) ||
      !std::isfinite(res.logLik))
    return done(fail("trgamma MLE failed to converge."));
  std::vector<std::string> fixed;
  if (scale.supplied) fixed.push_back("scale");
  if (shape1.supplied) fixed.push_back("shape1");
  if (shape2.supplied) fixed.push_back("shape2");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
