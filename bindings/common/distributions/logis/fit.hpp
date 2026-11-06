#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/logis/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace umvl_binding {
inline FitResult fit_logis(cppunivariate::span x, Held location_arg = {}, Held scale_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
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
  const Held location = location_arg;
  const Held scale = scale_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_location=location.supplied?location.value:nan;
  if(location.supplied && (!std::isfinite(held_location)))return fail("`location` must be finite.");
  const double held_scale=scale.supplied?scale.value:nan;
  if(scale.supplied && (!std::isfinite(held_scale) || !(held_scale>0)))return fail("`scale` must be positive and finite.");
  std::vector<std::string> fixed;
  if(location.supplied)fixed.push_back("location");
  if(scale.supplied)fixed.push_back("scale");

  if(location.supplied && !scale.supplied && std::all_of(x.begin(),x.end(),[&](double v){return v==held_location;}))
    return classify(fail("Logistic scale likelihood is unbounded: all observations equal the held location."),{},"nomle",{});
  cppunivariate::LogisticResult res =
      cppunivariate::logistic_mle(x,abstol,iterlim,held_location,held_scale);
  if (!scale.supplied && !x.empty() && std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }) && (!location.supplied || location.value == x[0]))
    return classify(fail("Logistic likelihood is unbounded for constant data at the fitted or held location."),{},"nomle",{});
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    return fail("Logistic MLE failed: non-finite or invalid parameters");
  if(res.status==cppunivariate::LogisticStatus::IterationFailure)return fail("Conditional fit did not converge; increase iterlim.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"location",res.mu},{"scale",res.scale}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }();

}
}
