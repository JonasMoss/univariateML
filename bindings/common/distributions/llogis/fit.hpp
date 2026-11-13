#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/llogis/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>
namespace umvl_binding {
inline FitResult fit_llogis(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
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

  cppunivariate::LogLogisticResult res =
      cppunivariate::loglogistic_mle(x, abstol, iterlim, shape_held, scale_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };

  if (res.status == cppunivariate::LogLogisticStatus::InvalidInput)
    return done(fail("Log-logistic requires nonempty positive finite data, valid held parameters, positive abstol and positive iterlim."));
  if (res.status == cppunivariate::LogLogisticStatus::Unbounded)
    return done(nomle("The log-logistic maximum likelihood estimator does not exist: the likelihood is unbounded for these constant transformed observations."));
  if (res.status == cppunivariate::LogLogisticStatus::IterationFailure)
    return done(fail("Log-logistic MLE failed to converge or cannot resolve the fitted coefficients and likelihood in double precision."));
  if (!(res.shape>0.0) || !(res.scale>0.0) || !std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    if (!shape.supplied && !scale.supplied && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
      return done(fail("The log-logistic maximum likelihood estimator does not exist for constant data: the likelihood is unbounded."));
    return done(fail("Log-logistic MLE failed: data exceed double precision."));
  }
  return done([&]() { FitResult out; if(!res.certified)out.extra.push_back({"certified",0}); out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
