#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/naka/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>
namespace umvl_binding {
inline FitResult fit_naka(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
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
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, "nomle", {}); };
  const Held shape = shape_arg;
  const Held scale = scale_arg;

  std::vector<std::string> fixed;
  const double shape_held = !shape.supplied ? std::numeric_limits<double>::quiet_NaN() : shape.value;
  if (shape.supplied) {
    if (!(std::isfinite(shape_held) && shape_held > 0.0)) return fail("Held `shape` must be positive and finite.");
    fixed.push_back("shape");
  }
  const double scale_held = !scale.supplied ? std::numeric_limits<double>::quiet_NaN() : scale.value;
  if (scale.supplied) {
    if (!(std::isfinite(scale_held) && scale_held > 0.0)) return fail("Held `scale` must be positive and finite.");
    fixed.push_back("scale");
  }

  auto res = cppunivariate::naka_mle(x, abstol, iterlim, shape_held, scale_held);
  if (res.status == cppunivariate::GammaStatus::InvalidInput) return fail("Nakagami requires nonempty positive finite data and valid held parameters.");
  if (res.status == cppunivariate::GammaStatus::TransformFailure) return fail("naka MLE failed: transformed sufficient statistics or fitted coefficients cannot be resolved in double precision.");
  if (res.status == cppunivariate::GammaStatus::IterationFailure)
    return fail("Gamma delegate optimizer IterationFailure; increase iterlim.");
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    return nomle("The Nakagami maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as the shape grows.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    return fail("Nakagami MLE failed");
  }
  return [&]() { FitResult out; if(!res.certified)out.extra.push_back({"certified",0}); out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
