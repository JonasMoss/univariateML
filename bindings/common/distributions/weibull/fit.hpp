#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include <limits>
#include <algorithm>
#include <cmath>
#include <vector>
namespace umvl_binding {
inline FitResult fit_weibull(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, Held shape0 = {}, double reltol = 1e-10, int iterlim = 100) {
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
  const Held shape = shape_arg;
  const Held scale = scale_arg;

  std::vector<std::string> fixed;
  const double shape_held = !shape.supplied ? std::numeric_limits<double>::quiet_NaN() : shape.value;
  if (shape.supplied) {
    if (!(std::isfinite(shape_held) && shape_held > 0.0)) return invalid_argument(fail("Held `shape` must be positive and finite."));
    fixed.push_back("shape");
  }
  const double scale_held = !scale.supplied ? std::numeric_limits<double>::quiet_NaN() : scale.value;
  if (scale.supplied) {
    if (!(std::isfinite(scale_held) && scale_held > 0.0)) return invalid_argument(fail("Held `scale` must be positive and finite."));
    fixed.push_back("scale");
  }

  double start = std::numeric_limits<double>::quiet_NaN();
  if (shape0.supplied) {
    start = shape0.value;
    if (!(start > 0.0) || !std::isfinite(start)) {
      return invalid_argument(fail("`shape0` must be a positive finite number."));
    }
  }
  auto res = cppunivariate::weibull_mle(x, reltol, iterlim, start, shape_held, scale_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };

  if(res.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("weibull numerical accuracy failure: requested coefficient accuracy cannot be established."),res.stop_detail,outcome_kind(o),{"shape","scale"}));
  if (res.status==cppunivariate::WeibullStatus::AccuracyFailure)
    return done(stop_failure(fail("Weibull requested coefficient accuracy cannot be represented after rescaling."),res.stop_detail,outcome_kind(o),{"shape","scale"}));
  if (res.status == cppunivariate::WeibullStatus::InvalidInput)
    return done(fail("Weibull requires nonempty positive finite data, valid held parameters, positive reltol and positive iterlim."));
  if (res.status == cppunivariate::WeibullStatus::Unbounded)
    return done(nomle("The weibull maximum likelihood estimator does not exist: the likelihood is unbounded for constant data at the fitted or held scale."));
  if (res.status == cppunivariate::WeibullStatus::IterationFailure)
    return done(fail("The held-parameter weibull score solver failed to converge; increase iterlim."));
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    if (!shape.supplied && !scale.supplied && std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      return done(fail("The Weibull maximum likelihood estimator does not exist for constant data: the likelihood is unbounded."));
    return done(fail("Weibull MLE failed"));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
