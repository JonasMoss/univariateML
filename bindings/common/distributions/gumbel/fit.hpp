#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gumbel/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace umvl_binding {
inline FitResult fit_gumbel(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, double reltol = 1e-10, int iterlim = 100) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(reltol>0 && std::isfinite(reltol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mu=mu.supplied?mu.value:nan;
  if(mu.supplied && (!std::isfinite(held_mu)))return invalid_argument(fail("`mu` must be finite."));
  const double held_sigma=sigma.supplied?sigma.value:nan;
  if(sigma.supplied && (!std::isfinite(held_sigma) || !(held_sigma>0)))return invalid_argument(fail("`sigma` must be positive and finite."));
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");

  cppunivariate::GumbelResult res = cppunivariate::fit_gumbel_held(x,reltol,iterlim,held_mu,held_sigma);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::GumbelStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(res.status));
    // Preserve the classifier's refusal of a structured stop (invalid context
    // or missing finite payload); the core result does not retain that context.
    if (!out.error.empty() && out.error_kind.empty() && o.kind != cppunivariate::OutcomeKind::Failure)
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::Evaluation), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };
  if (res.status==cppunivariate::GumbelStatus::AccuracyFailure)
    return done(stop_failure(fail("Gumbel requested coefficient accuracy cannot be represented after rescaling."),res.stop_detail,outcome_kind(o),{"mu","sigma"}));
  if (res.status == cppunivariate::GumbelStatus::InvalidInput) return done(fail("Gumbel requires nonempty finite data, valid held parameters, a positive tolerance and positive iterlim."));
  if (res.status == cppunivariate::GumbelStatus::Unbounded) return done(nomle("The Gumbel maximum likelihood estimator does not exist for constant data: the likelihood is unbounded."));
  if (res.status == cppunivariate::GumbelStatus::IterationFailure)
    return done(fail("Gumbel optimizer IterationFailure; increase iterlim."));
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) || !std::isfinite(res.logLik)) {
    if (std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      return done(fail("The Gumbel maximum likelihood estimator does not exist for constant data: the likelihood is unbounded."));
    return done(fail("Gumbel MLE failed"));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",res.mu},{"sigma",res.sigma}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }());

}
}
