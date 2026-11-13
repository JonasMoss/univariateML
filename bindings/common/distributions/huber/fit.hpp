#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/huber/mle.hpp"
#include <limits>
namespace umvl_binding {
inline FitResult fit_huber(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held epsilon_arg = {}, double gradtol = 1e-10, int iterlim = 200) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(gradtol>0 && std::isfinite(gradtol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;
  const Held epsilon = epsilon_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double m=mu.supplied ? mu.value : nan;
  const double s=sigma.supplied ? sigma.value : nan;
  if (mu.supplied && !std::isfinite(m)) return invalid_argument(fail("`mu` must be finite."));
  if (sigma.supplied && (!(s>0) || !std::isfinite(s)))
    return invalid_argument(fail("`sigma` must be positive and finite."));
  if (epsilon.supplied && epsilon.length!=1) return invalid_argument(fail("`epsilon` must be a scalar or NULL."));
  const double e=epsilon.supplied ? epsilon.value : nan;
  if (epsilon.supplied && (!(e>0) || !std::isfinite(e)))
    return invalid_argument(fail("`epsilon` must be positive and finite."));
  auto r=cppunivariate::huber_mle(x,gradtol,iterlim,e,m,s);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(r.status));
    return with_outcome(std::move(out), o, static_cast<int>(r.status));
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };

  if(r.stop_detail.budget_exhausted)return done(classify(fail("Huber certificate exhausted its work budget; raising `iterlim` may resolve it."),r.stop_detail,outcome_kind(o),{"mu","sigma","epsilon"}));
  if(r.stop_detail.comparison_tie)return done(classify(fail("Huber MLE failed to converge."),r.stop_detail,"tie",{"mu","sigma","epsilon"}));
  if (r.status==cppunivariate::HuberStatus::InvalidData)
    return done(fail("Huber requires finite data, positive finite `gradtol` and positive `iterlim`."));
  if (r.status==cppunivariate::HuberStatus::UnboundedLikelihood)
    return done(nomle("Huber likelihood is unbounded for constant data at the fitted or held location."));
  if (r.status==cppunivariate::HuberStatus::NormalLimit)
    return done(classify(fail("Huber shape reaches the normal face; use `mlnorm`."),r.stop_detail,outcome_kind(o),{"mean","sd"},"norm"));
  if (r.status==cppunivariate::HuberStatus::LaplaceLimit)
    return done(classify(fail("Huber shape reaches the Laplace face; use `mllaplace`."),r.stop_detail,outcome_kind(o),{"mu","sigma"},"laplace"));
  if(r.status==cppunivariate::HuberStatus::IterationFailure &&
      (!r.stop_detail.candidates.empty() || r.stop_detail.loglik_upper==std::numeric_limits<double>::infinity()))
    return done(classify(fail("Huber MLE failed to converge."),r.stop_detail,outcome_kind(o),{"mu","sigma","epsilon"}));
  if (r.status!=cppunivariate::HuberStatus::Ok || !std::isfinite(r.logLik))
    return done(fail("Huber MLE failed to converge."));
  std::vector<std::string> fixed;
  if (epsilon.supplied) fixed.push_back("epsilon");
  if (mu.supplied) fixed.push_back("mu");
  if (sigma.supplied) fixed.push_back("sigma");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma},{"epsilon",r.epsilon}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
