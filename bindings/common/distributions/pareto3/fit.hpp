#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/pareto3/mle.hpp"
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_pareto3(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, Held min_arg = {}, double reltol = 1e-10, int iterlim = 1000) {
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
 auto res=cppunivariate::pareto3_mle(x,min,reltol,iterlim,shape.supplied?shape.value:NAN,scale.supplied?scale.value:NAN);
  if(res.status==cppunivariate::Pareto3Status::IterationFailure) return fail("Pareto III delegate search failed to converge.");
  if (!(res.shape>0.0) || !((1/res.scale)>0.0) || !std::isfinite(res.shape) || !std::isfinite(res.scale) || !(res.scale>0) || !std::isfinite(res.logLik)) {
    if (std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
      return fail("The log-logistic maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    return fail("Log-logistic MLE failed: data exceed double precision.");
  }
  return [&]() { FitResult out; if(!res.certified)out.extra.push_back({"certified",0}); out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale},{"min",min}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
