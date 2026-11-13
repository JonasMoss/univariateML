#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/invgamma/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>
namespace umvl_binding {
inline FitResult fit_invgamma(cppunivariate::span x, Held alpha_arg = {}, Held beta_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(abstol>0 && std::isfinite(abstol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held alpha = alpha_arg;
  const Held beta = beta_arg;

  std::vector<std::string> fixed;
  const double alpha_held = !alpha.supplied ? std::numeric_limits<double>::quiet_NaN() : alpha.value;
  if (alpha.supplied) {
    if (!(std::isfinite(alpha_held) && alpha_held > 0.0)) return invalid_argument(fail("Held `alpha` must be positive and finite."));
    fixed.push_back("alpha");
  }
  const double beta_held = !beta.supplied ? std::numeric_limits<double>::quiet_NaN() : beta.value;
  if (beta.supplied) {
    if (!(std::isfinite(beta_held) && beta_held > 0.0)) return invalid_argument(fail("Held `beta` must be positive and finite."));
    fixed.push_back("beta");
  }

  auto res = cppunivariate::invgamma_mle(x, abstol, iterlim, alpha_held, beta_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (res.status == cppunivariate::GammaStatus::Ok)
        return overridden(std::move(out), static_cast<int>(res.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(res.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };
  if (res.status == cppunivariate::GammaStatus::InvalidInput) return done(fail("Inverse gamma requires nonempty positive finite data and valid held parameters."));
  if (res.status == cppunivariate::GammaStatus::TransformFailure) return done(fail("invgamma MLE failed: transformed sufficient statistics or fitted coefficients cannot be resolved in double precision."));
  if (res.status == cppunivariate::GammaStatus::IterationFailure)
    return done(fail("Gamma delegate optimizer IterationFailure; increase iterlim."));
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    return done(nomle("The inverse gamma maximum likelihood estimator does not exist for constant "
               "data: the likelihood is unbounded as the shape grows."));
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    return done(fail("Inverse gamma MLE failed"));
  }
  return done([&]() { FitResult out; if(!res.certified)out.extra.push_back({"certified",0}); out.warnings = warnings; out.estimates = {{"alpha",res.shape},{"beta",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
