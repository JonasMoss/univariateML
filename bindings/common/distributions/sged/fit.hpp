#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/sged/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_sged(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}, Held nu_arg = {}, Held xi_arg = {}, double gradtol = 1e-10, int iterlim = 200) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(gradtol>0 && std::isfinite(gradtol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!(kind=="budget" && detail.budget_exhausted) && !finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held sd = sd_arg;
  const Held nu = nu_arg;
  const Held xi = xi_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.supplied ? mean.value : nan;
  if(mean.supplied && (!std::isfinite(held_mean))) return invalid_argument(fail("Invalid held `mean`: requires a finite value."));
  const double held_sd=sd.supplied ? sd.value : nan;
  if(sd.supplied && (!(std::isfinite(held_sd) && held_sd>0))) return invalid_argument(fail("Invalid held `sd`: requires a finite value in its parameter domain."));
  const double held_nu=nu.supplied ? nu.value : nan;
  if(nu.supplied && (!(std::isfinite(held_nu) && held_nu>0))) return invalid_argument(fail("Invalid held `nu`: requires a finite value in its parameter domain."));
  const double held_xi=xi.supplied ? xi.value : nan;
  if(xi.supplied && (!(std::isfinite(held_xi) && held_xi>0))) return invalid_argument(fail("Invalid held `xi`: requires a finite value in its parameter domain."));
  auto res=cppunivariate::sged_mle(x,gradtol,iterlim,held_mean,held_sd,held_nu,held_xi);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::SgedStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
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

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };
  if(res.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("sged numerical accuracy failure: requested coefficient accuracy cannot be established."),res.stop_detail,outcome_kind(o),{"mean","sd","nu","xi"}));
  if(res.accuracy_failure)return done(stop_failure(fail("sged numerical accuracy failure: final Newton refinement did not converge to the requested coefficient tolerance; increase iterlim or inspect conditioning."),res.stop_detail,outcome_kind(o),{"mean","sd","nu","xi"}));
  if(res.status==cppunivariate::SgedStatus::CertificateFailure) {
    const std::vector<std::string> names{"mean","sd","nu","xi"};
    if(res.stop_detail.budget_exhausted)
      return done(classify(fail("SGED certificate budget exhausted; increase `iterlim`."),res.stop_detail,outcome_kind(o),names));
    if(res.stop_detail.comparison_tie)
      return done(classify(fail("SGED maxima are tied or numerically indistinguishable; no unique MLE returned."),res.stop_detail,"tie",names));
    return done(classify(fail("SGED certificate numerical failure (arithmetic or root enclosure); increase `iterlim`."),res.stop_detail,outcome_kind(o),names));
  }
  if(res.status==cppunivariate::SgedStatus::UnsupportedHold) return done(fail("Unsupported conditional fit."));
  if(res.status==cppunivariate::SgedStatus::Unbounded) return done(nomle("Scale collapse: likelihood is unbounded at the held mean; no MLE."));
  if(res.status==cppunivariate::SgedStatus::ScaleBoundary) return done(fail("Scale-collapse or constrained uniform endpoint boundary could not be certified; no finite MLE returned."));
  if (res.status == cppunivariate::SgedStatus::InvalidStart) {
    if(!sd.supplied && gradtol>0 && std::isfinite(gradtol) && iterlim>0 && !x.empty() && std::isfinite(x[0]) && (!mean.supplied || held_mean==x[0]) && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return done(classify(fail("Skew GED MLE requires at least 2 finite observations with "
               "positive variance."),{},"nomle",{}));
    return done(fail("Skew GED MLE requires at least 2 finite observations with "
               "positive variance."));
  }
  const auto face_fail = [&](const std::string& message, const std::string& model, const std::vector<std::string>& names) {
    if(res.stop_detail.comparison_tie)return done(classify(fail(message),res.stop_detail,"tie",names));
    if(res.stop_detail.limit_coef.empty())return done(fail(message));
    return done(classify(fail(message),res.stop_detail,outcome_kind(o),names,model));
  };
  if (res.status == cppunivariate::SgedStatus::UniformBoundary) {
    return done(face_fail("SGED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.","unif",{"a","b"}));
  }
  if (res.status == cppunivariate::SgedStatus::RightHalfGedBoundary) {
    if(mean.supplied || sd.supplied)
      return done(face_fail("The likelihood is maximised by a translated half-GED with endpoint constrained by the held moments; no MLE in the skewed family.","unavailable:half-ged",{"mu","scale","nu"}));
    return done(face_fail("The likelihood is maximised by a translated half-GED with mode at the sample minimum; no MLE in the skewed family.","unavailable:half-ged",{"mu","scale","nu"}));
  }
  if (res.status == cppunivariate::SgedStatus::LeftHalfGedBoundary) {
    if(mean.supplied || sd.supplied)
      return done(face_fail("The likelihood is maximised by a translated half-GED with reflected endpoint constrained by the held moments; no MLE in the skewed family.","unavailable:reflected-half-ged",{"mu","scale","nu"}));
    return done(face_fail("The likelihood is maximised by a translated half-GED with mode at the sample maximum; no MLE in the skewed family.","unavailable:reflected-half-ged",{"mu","scale","nu"}));
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu) || !std::isfinite(res.xi)) {
    return done(fail("Skew GED MLE failed to converge (check inputs)"));
  }
  if(res.status==cppunivariate::SgedStatus::BfgsFailure) return done(fail("Skew fit optimizer failed its score check; increase `iterlim`."));
  std::vector<std::string> fixed;
  if(mean.supplied) fixed.push_back("mean");
  if(sd.supplied) fixed.push_back("sd");
  if(nu.supplied) fixed.push_back("nu");
  if(xi.supplied) fixed.push_back("xi");
  std::vector<std::pair<std::string,std::string>> restricted;
  if(res.restricted & 1u) restricted.emplace_back("mean","lower");
  if(res.restricted & 2u) restricted.emplace_back("mean","upper");
  if(res.restricted & 4u) restricted.emplace_back("sd","lower");
  if(res.restricted & 8u) restricted.emplace_back("sd","upper");
  if(res.restricted & 16u) restricted.emplace_back("nu","lower");
  if(res.restricted & 32u) restricted.emplace_back("nu","upper");
  if(res.restricted & 64u) restricted.emplace_back("xi","lower");
  if(res.restricted & 128u) restricted.emplace_back("xi","upper");
  return done([&]() { FitResult out; out.extra.push_back({"certified",res.certified?1.:0.}); out.warnings = warnings; out.estimates = {{"mean",res.mu},{"sd",res.sigma},{"nu",res.nu},{"xi",res.xi}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
