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
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held scale = scale_arg;

 if(scale.supplied && (!(scale.value>0)||!std::isfinite(scale.value))) return fail("`scale` must be positive and finite.");

  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double a = shape1.supplied ? shape1.value : nan;
  const double b = shape2.supplied ? shape2.value : nan;
  if ((shape1.supplied && (!(a > 0.0) || !std::isfinite(a))) ||
      (shape2.supplied && (!(b > 0.0) || !std::isfinite(b))))
    return fail("`shape1` and `shape2` must be positive finite numbers when supplied.");
  const cppunivariate::span xs = x;
  auto res = scale.supplied ? cppunivariate::invtrgamma_mle_fixed_scale(xs,scale.value,a,b,reltol,iterlim) : cppunivariate::invtrgamma_mle(xs, reltol, iterlim, a, b);
  if(res.stop_detail.budget_exhausted)return classify(fail("Constrained likelihood search failed to converge. Raising iterlim may resolve this stop."),res.stop_detail,"budget",{"shape1","shape2","scale"});
  if (res.status == cppunivariate::InvTrGammaStatus::InvalidData)
    return (x.size()>=2 && std::isfinite(x[0]) && !scale.supplied && (std::isnan(a)||std::isnan(b)) && reltol>0 && std::isfinite(reltol) && iterlim>0 && x[0]>0 && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) ? classify(fail("invtrgamma requires at least two distinct positive finite observations, "
               "positive `reltol` and positive `iterlim`; constant samples can have unbounded likelihood."),{},"nomle",{}) : fail("invtrgamma requires at least two distinct positive finite observations, "
               "positive `reltol` and positive `iterlim`; constant samples can have unbounded likelihood.");
  if (res.status == cppunivariate::InvTrGammaStatus::LognormalBoundary)
    return classify(fail("The invtrgamma likelihood is maximised at a limit. Use `mllnorm` to fit this model."),res.stop_detail,"limit",{"meanlog","sdlog"},"lnorm");
  if (res.status == cppunivariate::InvTrGammaStatus::ParetoBoundary && scale.supplied)
    return classify(fail("The held-scale likelihood selects the limiting fixed-endpoint Pareto face at scale; no matching held-endpoint fitter is available."),res.stop_detail,"limit",{"a","b"},"pareto");
  if (res.status == cppunivariate::InvTrGammaStatus::ParetoBoundary)
    return classify(fail("The invtrgamma likelihood is maximised at a limit. Use `mlpareto` to fit this model."),res.stop_detail,"limit",{"a","b"},"pareto");
  if (res.status == cppunivariate::InvTrGammaStatus::ScaleOutOfRange) {
    const double gap = res.logLik - cppunivariate::lognormal_mle(xs).logLik;
    return fail("The invtrgamma maximum likelihood fit lies so close to the lognormal limit "
               "that `scale` is not representable with normal double precision (log(scale) = %g). "
               "Use `mllnorm`: its log-likelihood is lower by only %g.",
               res.log_scale, gap);
  }
  if (res.status == cppunivariate::InvTrGammaStatus::IterationFailure) return fail("Constrained likelihood search failed to converge.");
  if (!std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale) ||
      !std::isfinite(res.logLik))
    return fail("invtrgamma MLE failed to converge.");
  std::vector<std::string> fixed;
  if (scale.supplied) fixed.push_back("scale");
  if (shape1.supplied) fixed.push_back("shape1");
  if (shape2.supplied) fixed.push_back("shape2");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
