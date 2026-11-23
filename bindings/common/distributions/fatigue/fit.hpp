#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/fatigue/mle.hpp"
#include "cppunivariate/distributions/fatigue/dpqr.hpp"
#include <cmath>
#include <algorithm>
#include <limits>
namespace umvl_binding {
inline FitResult fit_fatigue(cppunivariate::span x, Held alpha_arg = {}, Held beta_arg = {}, Held mu_arg = {}, double tol = 1e-10, int iterlim = 100) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held alpha = alpha_arg;
  const Held beta = beta_arg;
  const Held mu = mu_arg;

  const double threshold = mu.supplied?mu.value:NAN;
  if (mu.supplied && !std::isfinite(threshold)) return invalid_argument(fail("`mu` must be finite."));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=alpha.supplied?alpha.value:nan;
  const double b=beta.supplied?beta.value:nan;
  if ((alpha.supplied&&(!(a>0)||!std::isfinite(a))) ||
      (beta.supplied&&(!(b>0)||!std::isfinite(b))))
    return invalid_argument(fail("Held `alpha` and `beta` must be positive finite numbers."));
  auto res = cppunivariate::fatigue_mle_shifted(x, threshold, tol, iterlim,a,b);
  const auto o = cppunivariate::outcome(res);
  const auto chosen = cppunivariate::comparison_choice(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"alpha",alpha_arg.supplied},{"beta",beta_arg.supplied},{"mu",mu_arg.supplied}});
  };
  if(res.status==cppunivariate::FatigueStatus::Unsupported)return done(failure("The threshold mu cannot be estimated. With mu free, the likelihood is unbounded for every sample with a unique minimum, so no maximum likelihood estimate exists. Supply mu."));


  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };

  if (res.status == cppunivariate::FatigueStatus::InvalidInput)
    return done(fail("Fatigue observations must be finite and strictly above `mu`."));
  if(res.transformation_failure)
    return done(fail("Fatigue optimizer SearchFailure: profile search did not converge or floating-point coefficient resolution failed; increase iterlim for unresolved searches."));
  if (res.status == cppunivariate::FatigueStatus::SearchFailure) {
    for(auto& candidate:res.stop_detail.candidates)candidate.coef.push_back(threshold);
    if(res.stop_detail.budget_exhausted)return done(stop_failure(fail("Fatigue optimizer SearchFailure: certificate budget exhausted; increase iterlim."),res.stop_detail,outcome_payload(o),{"alpha","beta","mu"}));
    if(res.maximizers.size) {
      auto out = comparison_payload(stop_failure(failure(""),res.stop_detail,
          PayloadKind::Comparison,{"alpha","beta","mu"}),o,0,{{"alpha",alpha_arg},{"beta",beta_arg},{"mu",mu_arg}});
      out.candidates.clear();
      attach_maximizer_set(out, res.maximizers, "fatigue", [&](const auto& c) {
        return x.size()*cppunivariate::distributions::dfatigue(x[0],c[0],c[1],c[2],true);
      });
      return done(std::move(out));
    }
    if(res.stop_detail.comparison_tie)return done(comparison_payload(stop_failure(fail("Fatigue optimizer SearchFailure: competing global maxima cannot be distinguished."),res.stop_detail,PayloadKind::Comparison,{"alpha","beta","mu"}),o,chosen,{{"alpha",alpha_arg},{"beta",beta_arg},{"mu",mu_arg}}));
    return done(stop_failure(fail("Fatigue optimizer SearchFailure: profile search did not converge or floating-point coefficient resolution failed; increase iterlim for unresolved searches."),res.stop_detail,outcome_payload(o),{"alpha","beta","mu"}));
  }
  if (!(res.alpha>0.0) || !std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    if (res.status==cppunivariate::FatigueStatus::Unbounded)
      return done(nomle("The fatigue maximum likelihood estimator does not exist for constant data: the likelihood is unbounded."));
    return done(fail("Fatigue MLE failed: data exceed double precision or profile search did not converge; increase iterlim."));
  }
  std::vector<std::string> fixed;
  if(alpha.supplied) fixed.push_back("alpha");
  if(beta.supplied) fixed.push_back("beta");
  fixed.push_back("mu");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"alpha",res.alpha},{"beta",res.beta},{"mu",threshold}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
