#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/burr/mle.hpp"
namespace umvl_binding {
inline FitResult fit_burr(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held scale_arg = {}, double gradtol = 1e-8, int iterlim = 1000, Held shape10 = {}, Held shape20 = {}, Held scale0 = {}, double reltol = 1e-12, double abstol = 1.220703e-4) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held scale = scale_arg;

 if(shape1.supplied && (!(shape1.value>0)||!std::isfinite(shape1.value))) return fail("`shape1` must be positive and finite.");
 if(shape2.supplied && (!(shape2.value>0)||!std::isfinite(shape2.value))) return fail("`shape2` must be positive and finite.");
 if(scale.supplied && (!(scale.value>0)||!std::isfinite(scale.value))) return fail("burr requires a positive finite fixed scale.");

 auto start = [](const Held &v) {
   return v.supplied ? v.value : NAN;
 };
 auto res = (shape1.supplied || shape2.supplied) ?
 cppunivariate::burr_mle_holds(x,start(shape1),start(shape2),start(scale),gradtol,iterlim) : scale.supplied ?
 cppunivariate::burr_mle_fixed_scale(x,start(scale),abstol,iterlim) :
 cppunivariate::burr_mle(x,gradtol,iterlim,
 start(shape10),start(shape20),start(scale0),reltol);
 if (res.status == cppunivariate::BurrStatus::Unbounded) return fail("Likelihood is unbounded for this constant sample; no MLE.");
 if (res.status == cppunivariate::BurrStatus::AllGeOne) return fail("The fixed-scale burr MLE does not exist when all observations >= scale. Use `mlpareto` to fit this model with endpoint scale.");
 if (res.status == cppunivariate::BurrStatus::WeibullBoundary) return fail("The burr likelihood is maximised at a limit. Use `mlweibull` to fit this model.");
 if (res.status == cppunivariate::BurrStatus::ParetoBoundary) return fail("The burr likelihood is maximised at a limit. Use `mlpareto` to fit this model.");
 if (res.status == cppunivariate::BurrStatus::IterationFailure) return fail("Constrained likelihood search failed to converge.");
 if (res.status == cppunivariate::BurrStatus::InvalidInit ||
 !std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale))
 return fail("burr MLE requires positive finite nonconstant data and positive starts and a positive finite fixed scale");
 std::vector<std::string> fixed;
 if(shape1.supplied) fixed.push_back("shape1");
 if(shape2.supplied) fixed.push_back("shape2");
 if(scale.supplied) fixed.push_back("scale");
 return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
