#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/pareto2/mle.hpp"
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_pareto2(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, Held min_arg = {}, double reltol = 1e-10, int iterlim = 1000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape = shape_arg;
  const Held scale = scale_arg;
  if (!min_arg.supplied) return failure("Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.");
  const double min = min_arg.value;

 if(shape.supplied && (!(shape.value>0)||!std::isfinite(shape.value))) return fail("`shape` must be positive and finite.");
 if(scale.supplied && (!(scale.value>0)||!std::isfinite(scale.value))) return fail("`scale` must be positive and finite.");

 if (!std::isfinite(min)) return fail("Held bounds must be finite and ordered.");
 for(double v:x) if (!(v>min)) return fail("Observations must lie strictly inside the held support.");
 std::vector<std::string> fixed{"min"};
 if(shape.supplied) fixed.push_back("shape");
 if(scale.supplied) fixed.push_back("scale");
 auto res=cppunivariate::pareto2_mle(x,min,reltol,iterlim,shape.supplied?shape.value:NAN,scale.supplied?scale.value:NAN);
  if (res.status == cppunivariate::LomaxStatus::ExponentialLimit) {
    return stop_failure(fail("The Lomax maximum likelihood estimator does not exist for data "
               "whose likelihood is maximized on the exponential face. Use `mlexp(x - min)` to fit an "
               "exponential distribution."),res.stop_detail,"limit",{"shape","scale","min"},"exp");
  }
  if(res.status==cppunivariate::LomaxStatus::IterationFailure) return stop_failure(fail("Certified Lomax delegate failed; no verified maximum."),res.stop_detail,res.certificate_failure==cppunivariate::LomaxCertificateFailure::Budget?"budget":"numerical",{"shape","scale","min"},"");
  if (res.status != cppunivariate::LomaxStatus::Ok || !std::isfinite(res.scale) || !(res.scale>0) ||
      !std::isfinite(res.shape)) {
    return fail("Lomax MLE failed to converge (check dispersion/inputs)");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale},{"min",min}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
