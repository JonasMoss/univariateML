#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/pareto4/mle.hpp"
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_pareto4(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held scale_arg = {}, Held min_arg = {}, double reltol = 1e-10, int iterlim = 1000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held scale = scale_arg;
  if (!min_arg.supplied) return failure("Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.");
  const double min = min_arg.value;

 if(shape1.supplied && (!(shape1.value>0)||!std::isfinite(shape1.value))) return fail("`shape1` must be positive and finite.");
 if(shape2.supplied && (!(shape2.value>0)||!std::isfinite(shape2.value))) return fail("`shape2` must be positive and finite.");

 if (!std::isfinite(min)) return fail("Held bounds must be finite and ordered.");
 for(double v:x) if (!(v>min)) return fail("Observations must lie strictly inside the held support.");
 std::vector<std::string> fixed{"min"};
 if(scale.supplied) { if(!(scale.value>0) || !std::isfinite(scale.value)) return fail("Held parameters must be positive and finite."); fixed.push_back("scale"); }
 if(shape1.supplied) fixed.push_back("shape1");
 if(shape2.supplied) fixed.push_back("shape2");
 auto res=cppunivariate::pareto4_mle(x,min,reltol,iterlim,scale.supplied ? scale.value : std::numeric_limits<double>::quiet_NaN(),shape1.supplied?shape1.value:NAN,shape2.supplied?shape2.value:NAN);
 if (res.status == cppunivariate::BurrStatus::Unbounded) return fail("Likelihood is unbounded for this constant sample; no MLE.");
 if (res.status == cppunivariate::BurrStatus::AllGeOne) return fail("The fixed-scale Pareto IV MLE does not exist when all observations >= min + scale. The limit is a shifted Pareto with its endpoint held at min + scale; the package has no held-endpoint Pareto fitter.");
 if (res.status == cppunivariate::BurrStatus::WeibullBoundary) return fail("The burr likelihood is maximised at a limit. Use `mlweibull(x - min)` to fit this model.");
 if (res.status == cppunivariate::BurrStatus::ParetoBoundary) return fail("The burr likelihood is maximised at a limit. Use `mlpareto(x - min)` to fit this model.");
 if (res.status == cppunivariate::BurrStatus::CertificateTie) return fail("Pareto IV certificate unresolved at a tie or sign-uncertain seam; no verified MLE.");
 if (res.status == cppunivariate::BurrStatus::CertificateBudget) return fail("Pareto IV certificate budget exhausted; no verified MLE.");
 if (res.status == cppunivariate::BurrStatus::NumericalFailure) return fail("Pareto IV certificate numerical failure; no verified MLE.");
 if (res.status == cppunivariate::BurrStatus::IterationFailure) return fail("Pareto IV delegate search failed to converge.");
 if (res.status == cppunivariate::BurrStatus::InvalidInit ||
 !std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale))
 return fail("burr MLE requires positive finite nonconstant data and positive starts and a positive finite fixed scale");
 return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"scale",res.scale},{"min",min}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
