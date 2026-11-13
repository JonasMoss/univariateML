#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/snorm/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_snorm(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}, Held xi_arg = {}, int iterlim = 200) {
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!(kind=="budget" && detail.budget_exhausted) && !finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  if(iterlim<=0)return invalid_argument(fail("Invalid `iterlim`: requires a positive integer."));
  const Held mean = mean_arg;
  const Held sd = sd_arg;
  const Held xi = xi_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.supplied ? mean.value : nan;
  if(mean.supplied && (!std::isfinite(held_mean))) return invalid_argument(fail("Invalid held `mean`: requires a finite value."));
  const double held_sd=sd.supplied ? sd.value : nan;
  if(sd.supplied && (!(std::isfinite(held_sd) && held_sd>0))) return invalid_argument(fail("Invalid held `sd`: requires a finite value in its parameter domain."));
  const double held_xi=xi.supplied ? xi.value : nan;
  if(xi.supplied && (!(std::isfinite(held_xi) && held_xi>0))) return invalid_argument(fail("Invalid held `xi`: requires a finite value in its parameter domain."));
  auto res=cppunivariate::snorm_mle(x,1.220703e-4,iterlim,held_mean,held_sd,held_xi);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (res.status == cppunivariate::SnormStatus::Ok)
        return overridden(std::move(out), static_cast<int>(res.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(res.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };
  if(res.status==cppunivariate::SnormStatus::Unbounded) return done(nomle("Scale collapse: the conditional skew-normal likelihood is unbounded; no MLE."));
  if(res.stop_detail.budget_exhausted)return done(classify(fail("Conditional skew-normal certificate budget exhausted; increase `iterlim`."),res.stop_detail,outcome_kind(o),{"mean","sd","xi"}));
  if(res.stop_detail.comparison_tie)return done(classify(fail("Conditional skew-normal fit did not converge."),res.stop_detail,"tie",{"mean","sd","xi"}));
  if(res.status==cppunivariate::SnormStatus::BfgsFailure) return done(classify(fail("Conditional skew-normal fit did not converge (certificate arithmetic or root enclosure; increase `iterlim`)."),res.stop_detail,outcome_kind(o),{"mean","sd","xi"}));
  // An unrepresentable face retains the classifier's existing plain error.
  const auto limit = [&](FitResult out, const std::string& model) {
    if (o.kind != cppunivariate::OutcomeKind::Limit) return done(std::move(out));
    return done(classify(std::move(out),res.stop_detail,outcome_kind(o),{"mu","sigma"},model));
  };
  if (res.status == cppunivariate::SnormStatus::RightHalfNormalBoundary) {
    if(mean.supplied || sd.supplied)return limit(fail("The conditional skew-normal supremum is a half-normal with endpoint constrained by the held moments; the MLE does not exist."),"hnorm");
    return limit(fail("The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal."),"hnorm");
  }
  if (res.status == cppunivariate::SnormStatus::LeftHalfNormalBoundary) {
    if(mean.supplied || sd.supplied)return limit(fail("The conditional skew-normal supremum is a reflected half-normal with endpoint constrained by the held moments; the MLE does not exist."),"unavailable:reflected-half-normal");
    return limit(fail("The likelihood is maximised by a translated half-normal (mode at the sample maximum); the skew-normal MLE does not exist. Use `mlhnorm` on `-x` (a half-normal reflected at the sample maximum)."),"unavailable:reflected-half-normal");
  }
  if (res.status == cppunivariate::SnormStatus::InvalidStart) {
    return done(fail("Skew-normal MLE requires at least 2 finite observations with "
               "positive variance."));
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.xi)) {
    return done(fail("Skew-normal MLE failed to converge (check inputs)"));
  }
  std::vector<std::string> fixed;
  if(mean.supplied) fixed.push_back("mean");
  if(sd.supplied) fixed.push_back("sd");
  if(xi.supplied) fixed.push_back("xi");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",res.mu},{"sd",res.sigma},{"xi",res.xi}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
