#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gumbel/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace umvl_binding {
inline FitResult fit_gumbel(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, double reltol = 1.220703e-4, int iterlim = 100) {
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
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, "nomle", {}); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mu=mu.supplied?mu.value:nan;
  if(mu.supplied && (!std::isfinite(held_mu)))return fail("`mu` must be finite.");
  const double held_sigma=sigma.supplied?sigma.value:nan;
  if(sigma.supplied && (!std::isfinite(held_sigma) || !(held_sigma>0)))return fail("`sigma` must be positive and finite.");
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");

  cppunivariate::GumbelResult res = cppunivariate::fit_gumbel_held(x,reltol,iterlim,held_mu,held_sigma);
  if (res.status == cppunivariate::GumbelStatus::InvalidInput) return fail("Gumbel requires nonempty finite data, valid held parameters, a positive tolerance and positive iterlim.");
  if (res.status == cppunivariate::GumbelStatus::Unbounded) return nomle("The Gumbel maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
  if (res.status == cppunivariate::GumbelStatus::IterationFailure)
    return fail("Gumbel optimizer IterationFailure; increase iterlim.");
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) || !std::isfinite(res.logLik)) {
    if (std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      return fail("The Gumbel maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    return fail("Gumbel MLE failed");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",res.mu},{"sigma",res.sigma}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }();

}
}
