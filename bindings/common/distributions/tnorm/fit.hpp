#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/tnorm/mle.hpp"
#include <cmath>
#include <limits>
#include <string>
namespace umvl_binding {
inline FitResult fit_tnorm(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}, Held a_arg = {}, Held b_arg = {}, double reltol = 1e-12, int iterlim = 100) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(reltol>0 && std::isfinite(reltol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, "nomle", {}); };
  const Held mean = mean_arg;
  const Held sd = sd_arg;
  const Held a = a_arg;
  const Held b = b_arg;

  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double lower = a.supplied ? a.value : nan;
  const double upper = b.supplied ? b.value : nan;
  if ((a.supplied && std::isnan(lower)) || (b.supplied && std::isnan(upper)))
    return fail("Truncated normal bounds must not be missing.");
  const double held_mean = mean.supplied ? mean.value : nan;
  const double held_sd = sd.supplied ? sd.value : nan;
  if (mean.supplied && !std::isfinite(held_mean))
    return fail("`mean` must be finite.");
  if (sd.supplied && !(std::isfinite(held_sd) && held_sd > 0))
    return fail("`sd` must be finite and positive.");
  const auto res = cppunivariate::tnorm_mle(x, lower, upper, reltol, iterlim,
                                            held_mean, held_sd);
  const std::string rate = binding_message("%.6g", res.limit_rate);
  const std::string decay = binding_message("%.6g", -res.limit_rate);
  switch (res.status) {
    case cppunivariate::TnormStatus::Ok: break;
    case cppunivariate::TnormStatus::InvalidBounds:
      return (x.size()>=1 && std::isfinite(x[0]) && !a.supplied && !b.supplied && !sd.supplied && (!mean.supplied || held_mean==x[0]) && reltol>0 && std::isfinite(reltol) && iterlim>0 && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) ? classify(fail("Truncated normal bounds must satisfy a < b, with a < Inf and b > -Inf."),{},"nomle",{}) : fail("Truncated normal bounds must satisfy a < b, with a < Inf and b > -Inf.");
    case cppunivariate::TnormStatus::OutsideBounds:
      return fail("Truncated normal observations must lie within the closed bounds [a, b].");
    case cppunivariate::TnormStatus::DegenerateData:
      return nomle("Truncated normal likelihood is unbounded as sd -> 0: all observations equal the fitted mean or the projection of the held mean onto [a, b].");
    case cppunivariate::TnormStatus::UnboundedLocation:
      return nomle("Truncated normal likelihood is unbounded with held `sd`: all observations equal a finite endpoint and the parent mean escapes beyond that endpoint.");
    case cppunivariate::TnormStatus::ExponentialBoundary:
      return classify(fail("The truncated normal fit on (a, Inf) escapes to, or is numerically tied with, a + Y, Y exponential with rate " + decay + ", as sd -> Inf; no finite MLE is returned (variance >= (mean(x) - a)^2, or a likelihood tie within 1e-8 per observation). Use `mlexp(x - a)` to fit the limit."),res.stop_detail,"limit",{"mu","rate"},"unavailable:translated-exponential");
    case cppunivariate::TnormStatus::ReflectedExponentialBoundary:
      return classify(fail("The truncated normal fit on (-Inf, b) escapes to, or is numerically tied with, b - Y, Y exponential with rate " + rate + ", as sd -> Inf; no finite MLE is returned (variance >= (b - mean(x))^2, or a likelihood tie within 1e-8 per observation). Use `mlexp(b - x)` to fit the limit."),res.stop_detail,"limit",{"mu","rate"},"unavailable:reflected-exponential");
    case cppunivariate::TnormStatus::TruncatedExponentialBoundary:
      return classify(fail("The truncated normal fit on (a, b) escapes to, or is numerically tied with, the truncated exponential distribution on (a, b) with density proportional to exp(" + rate + " * x) (uniform at rate 0) as sd -> Inf; no finite MLE is returned (an escape or a likelihood tie within 1e-8 per observation). The package has no model for this limit."),res.stop_detail,"limit",{"a","b","rate"},"unavailable:truncated-exponential");
    case cppunivariate::TnormStatus::UniformBoundary:
      return classify(fail("With `mean` held fixed, the truncated normal fit escapes to, or is numerically tied with, the uniform distribution on (a, b) as sd -> Inf; no finite MLE is returned (mean squared distance is at least the uniform's, or a likelihood tie within 1e-8 per observation)."),res.stop_detail,"limit",{"a","b"},"unif");
    case cppunivariate::TnormStatus::InvalidFixed:
      return fail("`mean` must be finite and `sd` finite and positive.");
    case cppunivariate::TnormStatus::IterationFailure:
      return fail("Truncated normal Newton iteration did not converge; increase `iterlim`.");
    case cppunivariate::TnormStatus::InvalidInput:
    default:
      return fail("Truncated normal MLE requires finite data, positive `reltol` and positive `iterlim`.");
  }
  std::vector<std::string> fixed;
  if (mean.supplied) fixed.push_back("mean");
  if (sd.supplied) fixed.push_back("sd");
  if (a.supplied) fixed.push_back("a");
  if (b.supplied) fixed.push_back("b");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",res.mean},{"sd",res.sd},{"a",res.a},{"b",res.b}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
