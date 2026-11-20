#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/betapr/mle.hpp"
#include <cmath>
namespace umvl_binding {
inline FitResult fit_betapr(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held scale_arg = {}, double tol = 1e-10, int iterlim = 2000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& message) { return failure(message, warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held scale = scale_arg;

  const double held = scale.supplied ? scale.value : 1.0;
  if (!(held > 0) || !std::isfinite(held))
    return invalid_argument(fail("`scale` must be a positive finite number."));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=shape1.supplied?shape1.value:nan;
  const double b=shape2.supplied?shape2.value:nan;
  if((shape1.supplied&&(!(a>0)||!std::isfinite(a))) ||
     (shape2.supplied&&(!(b>0)||!std::isfinite(b))))
    return invalid_argument(fail("Held `shape1` and `shape2` must be positive finite numbers."));
  auto res = (shape1.supplied||shape2.supplied)
      ? cppunivariate::betapr_mle_holds(x,a,b,scale.supplied?held:nan,tol,iterlim)
      : scale.supplied
      ? cppunivariate::betapr_mle_fixed_scale(x, held, tol, iterlim)
      : cppunivariate::betapr_mle(x, tol, iterlim);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"shape1",shape1_arg.supplied},{"shape2",shape2_arg.supplied},{"scale",scale_arg.supplied}});
  };
  if(res.status==cppunivariate::BetaPrimeStatus::ExpBoundary)
    return done(stop_failure(fail("The certified beta-prime submodel selects the exponential limit. Use `mlexp`."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},"exp"));
  if(res.status==cppunivariate::BetaPrimeStatus::InvExpBoundary)
    return done(stop_failure(fail("The certified beta-prime submodel selects the inverse exponential limit. Use `mlinvexp`."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},"invexp"));
  if (res.status == cppunivariate::BetaPrimeStatus::GammaBoundary)
    return done(stop_failure(fail("The beta-prime likelihood selects the gamma limit. Use `mlgamma` to fit this model."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},"gamma"));
  if (res.status == cppunivariate::BetaPrimeStatus::InvGammaBoundary)
    return done(stop_failure(fail("The beta-prime likelihood selects the inverse-gamma limit. Use `mlinvgamma` to fit this model."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},"invgamma"));
  if (res.status == cppunivariate::BetaPrimeStatus::BfgsFailure && res.certificate_failure==cppunivariate::LomaxCertificateFailure::Budget)
    return done(stop_failure(fail("Beta-prime BfgsFailure: global certificate budget exhausted (128 * min(iterlim, 100000) evaluations); raising iterlim may resolve this stop."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},""));
  if (res.status == cppunivariate::BetaPrimeStatus::BfgsFailure)
    return done(comparison_payload(stop_failure(fail(o.kind==cppunivariate::OutcomeKind::Limit ?
        "Beta-prime global certificate cannot separate the finite profile from the gamma and inverse-gamma limits; no verified MLE." :
        "Beta-prime optimizer BfgsFailure: profile did not converge or its global certificate is unresolved; increase iterlim. An unresolved gamma or inverse-gamma comparison does not select `mlgamma` or `mlinvgamma`."),res.stop_detail,res.stop_detail.comparison_tie?PayloadKind::Comparison:outcome_payload(o),{"shape1","shape2","scale"},""),o,cppunivariate::comparison_choice(res),{{"shape1",shape1_arg},{"shape2",shape2_arg},{"scale",scale_arg}}));
  if(res.status==cppunivariate::BetaPrimeStatus::Unbounded)
    return done(stop_failure(fail("Beta-prime finite MLEs require positive log-variance; constant positive observations have unbounded likelihood."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},""));
  if (res.status == cppunivariate::BetaPrimeStatus::InvalidStart)
    return done(fail("Beta-prime MLE requires positive finite observations with positive log-variance."));
  std::vector<std::string> fixed;
  if(shape1.supplied)fixed.push_back("shape1");
  if(shape2.supplied)fixed.push_back("shape2");
  if(scale.supplied)fixed.push_back("scale");
  return done([&]() { FitResult out;  out.warnings = warnings; out.estimates = {{"shape1",res.alpha},{"shape2",res.beta},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
