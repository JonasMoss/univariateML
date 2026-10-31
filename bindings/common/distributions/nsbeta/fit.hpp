#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/nsbeta/mle.hpp"
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_nsbeta(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held min_arg = {}, Held max_arg = {}, double reltol = 1e-10, int iterlim = 1000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  if (!min_arg.supplied) return failure("Estimating the threshold is not supported: endpoint singularities or unattained limit models prevent an unrestricted MLE (P0, A4).");
  const double min = min_arg.value;
  if (!max_arg.supplied) return failure("Estimating the threshold is not supported: endpoint singularities or unattained limit models prevent an unrestricted MLE (P0, A4).");
  const double max = max_arg.value;

 if(shape1.supplied && (!(shape1.value>0)||!std::isfinite(shape1.value))) return fail("`shape1` must be positive and finite.");
 if(shape2.supplied && (!(shape2.value>0)||!std::isfinite(shape2.value))) return fail("`shape2` must be positive and finite.");

 if (!std::isfinite(min) || !std::isfinite(max) || !(max>min) || !std::isfinite(max-min)) return fail("Held bounds must be finite and ordered.");
 for(double v:x) if (!(v>min) || !(v<max)) return fail("Observations must lie strictly inside the held support.");
 std::vector<std::string> fixed{"min", "max"};
 if(shape1.supplied) fixed.push_back("shape1");
 if(shape2.supplied) fixed.push_back("shape2");
 auto res=cppunivariate::nsbeta_mle(x,min,max,reltol,iterlim,shape1.supplied?shape1.value:NAN,shape2.supplied?shape2.value:NAN);
  if (res.status == cppunivariate::BetaStatus::InvalidInput) return fail("Nonstandard beta requires nonempty finite data strictly inside its held bounds.");
  if (res.status == cppunivariate::BetaStatus::Degenerate) {
    return fail("The beta maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as both shapes grow.");
  }
  if(res.status==cppunivariate::BetaStatus::IterationFailure) return fail("Nonstandard beta delegate search failed to converge.");
  if (!std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.logLik)) {
    return fail("Beta MLE failed: the data are too close to a point mass to resolve the shapes in double precision.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"min",min},{"max",max}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
