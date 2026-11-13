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
  if (!min_arg.supplied) return invalid_argument(failure("Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values."));
  const double min = min_arg.value;

 if(shape.supplied && (!(shape.value>0)||!std::isfinite(shape.value))) return invalid_argument(fail("`shape` must be positive and finite."));
 if(scale.supplied && (!(scale.value>0)||!std::isfinite(scale.value))) return invalid_argument(fail("`scale` must be positive and finite."));

 if (!std::isfinite(min)) return invalid_argument(fail("Held bounds must be finite and ordered."));
 for(double v:x) if (!(v>min)) return invalid_argument(fail("Observations must lie strictly inside the held support."));
 std::vector<std::string> fixed{"min"};
 if(shape.supplied) fixed.push_back("shape");
 if(scale.supplied) fixed.push_back("scale");
 auto res=cppunivariate::pareto3_mle(x,min,reltol,iterlim,shape.supplied?shape.value:NAN,scale.supplied?scale.value:NAN);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };

  if(res.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("pareto3 numerical accuracy failure: requested coefficient accuracy cannot be established."),res.stop_detail,outcome_kind(o),{"shape","scale","min"}));
  if(res.status==cppunivariate::Pareto3Status::AccuracyFailure)return done(stop_failure(fail("pareto3 numerical accuracy failure: coefficients cannot be represented at the requested accuracy."),res.stop_detail,outcome_kind(o),{"shape","scale","min"}));
  if(res.status==cppunivariate::Pareto3Status::IterationFailure) return done(fail("Pareto III delegate search failed to converge."));
  if (!(res.shape>0.0) || !((1/res.scale)>0.0) || !std::isfinite(res.shape) || !std::isfinite(res.scale) || !(res.scale>0) || !std::isfinite(res.logLik)) {
    if (std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
      return done(fail("The log-logistic maximum likelihood estimator does not exist for constant data: the likelihood is unbounded."));
    return done(fail("Log-logistic MLE failed: data exceed double precision."));
  }
  return done([&]() { FitResult out; out.extra.push_back({"certified",res.certified?1.:0.}); out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale},{"min",min}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
