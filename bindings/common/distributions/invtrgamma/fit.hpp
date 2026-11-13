#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/invtrgamma/mle.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_invtrgamma(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held scale_arg = {}, double reltol = 1e-10, int iterlim = 100) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(reltol>0 && std::isfinite(reltol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    auto payload=detail;
    if(kind=="budget" && payload.candidates.empty() &&
       cppunivariate::valid_stop_coefficients(payload.limit_model,payload.limit_coef) && std::isfinite(payload.limit_loglik))
      payload.candidates.push_back({payload.limit_coef,payload.limit_loglik,payload.limit_loglik,payload.limit_model});
    if(!finite_stop_payload(payload,kind))return out;
    return stop_failure(out,payload,kind,names,model);
  };
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
  auto res = scale.supplied ? cppunivariate::invtrgamma_mle_fixed_scale(xs,scale.value,a,b,reltol,iterlim) : cppunivariate::invtrgamma_mle(xs, reltol, iterlim, a, b);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::InvTrGammaStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(res.status));
    // The adapter's sample/hold checks distinguish collapse from invalid input.
    if (out.error_kind == "nomle" && o.kind != cppunivariate::OutcomeKind::Nonexistence)
      return with_outcome(std::move(out), cppunivariate::FitOutcome::nonexistence(cppunivariate::OutcomeReason::Unbounded), static_cast<int>(res.status));
    // Preserve the classifier's refusal of a structured stop (invalid context
    // or missing finite payload); the core result does not retain that context.
    if (!out.error.empty() && out.error_kind.empty() && o.kind != cppunivariate::OutcomeKind::Failure)
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::Evaluation), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
  if(res.stop_detail.budget_exhausted)return done(classify(fail("Constrained likelihood search failed to converge. Raising iterlim may resolve this stop."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"}));
  if(res.stop_detail.comparison_tie) {
    if(scale.supplied)return done(classify(fail("The held-scale interior maximum is too close to the fixed-endpoint Pareto face to certify its coefficients."),res.stop_detail,"tie",{"shape1","shape2","scale"}));
    return done(classify(fail(res.stop_detail.limit_model==cppunivariate::StopModel::Lnorm ?
      "The interior and lognormal likelihoods cannot be distinguished at the comparison tolerance. Use `mllnorm` for the limiting fit." :
      "The interior and endpoint likelihoods cannot be distinguished at the comparison tolerance. Use `mlpareto` for the limiting fit."),res.stop_detail,"tie",{"shape1","shape2","scale"}));
  }
  if(res.transformation_failure)return done(classify(fail("Distinct observations or the held endpoint became indistinguishable after taking logarithms."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"}));
  if(res.reconstruction_failure)return done(classify(fail("Cannot certify the likelihood after reconstructing the coefficients in double precision."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"}));
  if (res.status == cppunivariate::InvTrGammaStatus::InvalidData)
    return done((x.size()>=2 && std::isfinite(x[0]) && (res.logLik==INFINITY || (!scale.supplied && (std::isnan(a)||std::isnan(b)))) && reltol>0 && std::isfinite(reltol) && iterlim>0 && x[0]>0 && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) ? classify(fail("invtrgamma requires at least two distinct positive finite observations, "
               "positive `reltol` and positive `iterlim`; constant samples can have unbounded likelihood."),{},"nomle",{}) : fail("invtrgamma requires at least two distinct positive finite observations, "
               "positive `reltol` and positive `iterlim`; constant samples can have unbounded likelihood."));
  if (res.status == cppunivariate::InvTrGammaStatus::LognormalBoundary)
    return done(classify(fail("The invtrgamma likelihood is maximised at a limit. Use `mllnorm` to fit this model."),res.stop_detail,outcome_kind(o),{"meanlog","sdlog"},"lnorm"));
  if (res.status == cppunivariate::InvTrGammaStatus::ParetoBoundary && scale.supplied)
    return done(classify(fail("The held-scale likelihood selects the limiting fixed-endpoint Pareto face at scale; no matching held-endpoint fitter is available."),res.stop_detail,outcome_kind(o),{"a","b"},"pareto"));
  if (res.status == cppunivariate::InvTrGammaStatus::ParetoBoundary)
    return done(classify(fail("The invtrgamma likelihood is maximised at a limit. Use `mlpareto` to fit this model."),res.stop_detail,outcome_kind(o),{"a","b"},"pareto"));
  if (res.status == cppunivariate::InvTrGammaStatus::ScaleOutOfRange) {
    const double gap = res.logLik - cppunivariate::lognormal_mle(xs).logLik;
    auto detail=res.stop_detail;detail.loglik_upper=INFINITY;
    return done(classify(fail("The invtrgamma maximum likelihood fit lies so close to the lognormal limit "
               "that `scale` is not representable with normal double precision (log(scale) = %g). "
               "Use `mllnorm`: its log-likelihood is lower by only %g.",
               res.log_scale, gap),detail,outcome_kind(o),{"shape1","shape2","scale"}));
  }
  if (res.status == cppunivariate::InvTrGammaStatus::IterationFailure) return done(classify(fail("Constrained likelihood search failed to converge."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"}));
  if (!std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale) ||
      !std::isfinite(res.logLik))
    return done(fail("invtrgamma MLE failed to converge."));
  std::vector<std::string> fixed;
  if (scale.supplied) fixed.push_back("scale");
  if (shape1.supplied) fixed.push_back("shape1");
  if (shape2.supplied) fixed.push_back("shape2");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
