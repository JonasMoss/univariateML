#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/ged/mle.hpp"
#include <cmath>
#include <limits>
#include <algorithm>
#include <vector>
namespace umvl_binding {
inline FitResult fit_ged(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}, Held nu_arg = {}, double tol = 1e-10, int iterlim = 200) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held sd = sd_arg;
  const Held nu = nu_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<std::string> fixed;
  const double held_mean=mean.supplied?mean.value:nan;
  if(mean.supplied && (!std::isfinite(held_mean)))return invalid_argument(fail("Invalid held `mean`."));
  if(mean.supplied)fixed.push_back("mean");
  const double held_sd=sd.supplied?sd.value:nan;
  if(sd.supplied && (!std::isfinite(held_sd)||!(held_sd>0)))return invalid_argument(fail("Invalid held `sd`."));
  if(sd.supplied)fixed.push_back("sd");
  const double held_nu=nu.supplied?nu.value:nan;
  if(nu.supplied && (!std::isfinite(held_nu)||!(held_nu>0)))return invalid_argument(fail("Invalid held `nu`."));
  if(nu.supplied)fixed.push_back("nu");
  auto res = cppunivariate::ged_mle(x, tol, iterlim, held_mean, held_sd, held_nu);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::GedStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    if (res.status == cppunivariate::GedStatus::Ok && !o.uniqueness_known) {
      out.error = "The likelihood comparison between GED candidates could not be resolved.";
      out = stop_failure(std::move(out), res.stop_detail, PayloadKind::Comparison, {"mean","sd","nu"});
    }
    if (o.kind == cppunivariate::OutcomeKind::Estimate)
      attach_maximizer_set(out, res.maximizers, "ged", [&](const auto& c) {
        return cppunivariate::ged_detail::conditional_loglik(x, c[0], c[1], c[2]);
      });
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"mean",mean_arg.supplied},{"sd",sd_arg.supplied},{"nu",nu_arg.supplied}});
  };
  if(res.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("ged numerical accuracy failure: requested coefficient accuracy cannot be established."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if(res.status==cppunivariate::GedStatus::InvalidShape)return done(fail("Held `nu` must be at least 1 under the existing GED restriction."));
  if(res.status==cppunivariate::GedStatus::Unbounded)
    return done(stop_failure(fail("GED scale likelihood is unbounded: all observations equal the fitted or held location."),{},PayloadKind::Nonexistence,{}));
  if(res.status!=cppunivariate::GedStatus::Ok && res.stop_detail.budget_exhausted)return done(stop_failure(fail("GED shape certificate did not converge within its work budget; increase iterlim."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if(res.status==cppunivariate::GedStatus::UniformUnresolved)return done(stop_failure(fail("GED held-sd uniform comparison is unresolved by the shape certificate."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  // A comparison estimate is a proved tie's exact-location witness; its point
  // log likelihood is evaluated as an Ok conditional fit evaluates it.
  const auto comparison=[&](FitResult out) {
    out=comparison_payload(std::move(out),o,res.maximizers.size ? 0 : cppunivariate::comparison_choice(res),{{"mean",mean_arg},{"sd",sd_arg},{"nu",nu_arg}});
    if(o.kind==cppunivariate::OutcomeKind::Estimate && out.estimates.size()==3) {
      out.logLik=res.comparison_logLik;
      if (res.maximizers.size) out.candidates.clear();
    }
    return out;
  };
  if(res.status==cppunivariate::GedStatus::Nonunique)return done(comparison(stop_failure(fail("GED maxima are not unique or their comparison is unresolved, including a possible Laplace median interval."),res.stop_detail,PayloadKind::Comparison,{"mean","sd","nu"})));
  if(res.status!=cppunivariate::GedStatus::Ok && res.stop_detail.comparison_tie)return done(comparison(stop_failure(fail("Conditional GED search did not converge; increase iterlim."),res.stop_detail,PayloadKind::Comparison,{"mean","sd","nu"})));
  if (res.status == cppunivariate::GedStatus::InvalidStart) {
    return done(fail("GED MLE requires at least 2 finite observations with positive "
               "variance."));
  }
  if (res.status == cppunivariate::GedStatus::UniformBoundary) {
    return done(stop_failure(fail("GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead."),res.stop_detail,outcome_payload(o),{"a","b"},"unif"));
  }
  if(res.status==cppunivariate::GedStatus::TransformFailure)return done(stop_failure(fail("GED normalization loses coefficient or likelihood accuracy; no finite fit is certified."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if(res.status==cppunivariate::GedStatus::BfgsFailure)return done(stop_failure(fail("Conditional GED search did not converge; increase iterlim."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu)) {
    return done(fail("GED MLE failed to converge (check inputs)"));
  }
  std::vector<std::pair<std::string,std::string>> restricted;
  if(res.restricted & 1u) restricted.emplace_back("mean","lower");
  if(res.restricted & 2u) restricted.emplace_back("mean","upper");
  if(res.restricted & 4u) restricted.emplace_back("sd","lower");
  if(res.restricted & 8u) restricted.emplace_back("sd","upper");
  if(res.restricted & 16u) restricted.emplace_back("nu","lower");
  if(res.restricted & 32u) restricted.emplace_back("nu","upper");
  return done([&]() { FitResult out;  out.warnings = warnings; out.estimates = {{"mean",res.mu},{"sd",res.sigma},{"nu",res.nu}}; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }());

}
}
