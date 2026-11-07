#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
namespace umvl_binding {
inline FitResult fit_frechet(cppunivariate::span x, Held lambda_arg = {}, Held mu_arg = {}, Held sigma_arg = {}, double reltol=1e-10, int iterlim=100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held lambda = lambda_arg;
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

 if(mu.supplied && !std::isfinite(mu.value)) return fail("mu must be finite.");
 const double shape=lambda.supplied ? lambda.value : std::numeric_limits<double>::quiet_NaN();
 const double scale=sigma.supplied ? sigma.value : std::numeric_limits<double>::quiet_NaN();
 if(lambda.supplied && (!(shape>0) || !std::isfinite(shape))) return fail("lambda must be positive and finite.");
 if(sigma.supplied && (!(scale>0) || !std::isfinite(scale))) return fail("sigma must be positive and finite.");
 std::vector<std::string> fixed;
 if(lambda.supplied) fixed.push_back("lambda");
 if(mu.supplied) fixed.push_back("mu");
 if(sigma.supplied) fixed.push_back("sigma");
 const auto r=cppunivariate::frechet_mle(x,reltol,iterlim,
 mu.supplied ? mu.value : std::numeric_limits<double>::quiet_NaN(),shape,scale);
 if(r.status==cppunivariate::FrechetStatus::NumericalFailure ||
    (r.status==cppunivariate::FrechetStatus::IterationFailure && std::isinf(r.stop_detail.loglik_upper)))
   return stop_failure(fail("Frechet certificate failed to converge or resolve the maximum numerically; raise iterlim for an exhausted solve."),r.stop_detail,"numerical",{"lambda","mu","sigma"});
 if(r.status==cppunivariate::FrechetStatus::GumbelLimit) return fail("Frechet selects the Gumbel limit. Use `mlgumbel`.");
 if(r.status==cppunivariate::FrechetStatus::NoMLE || (r.status==cppunivariate::FrechetStatus::InvalidData && !std::isnan(r.stop_detail.loglik_upper))) return stop_failure(fail("Frechet has no finite maximum: minimum multiplicity permits scale collapse."),r.stop_detail,"nomle",{"lambda","mu","sigma"});
 if(r.status==cppunivariate::FrechetStatus::InvalidData) return fail("Frechet requires finite observations strictly above held mu and compatible holds. Free shape and scale require nonconstant data; the default free-mu fit requires at least three observations and admissible minimum tie multiplicity. Constant data can have an unbounded shape likelihood and fixed shape can have an unattained collapse face. Tolerance and iteration limit must be positive.");
 if(r.status!=cppunivariate::FrechetStatus::Ok) return fail("Frechet profile search failed to converge.");
  std::vector<std::pair<std::string,std::string>> restricted;
  if(r.restricted & 1u) restricted.emplace_back("lambda","lower");
  if(r.restricted & 2u) restricted.emplace_back("lambda","upper");
  if(r.restricted & 4u) restricted.emplace_back("mu","lower");
  if(r.restricted & 8u) restricted.emplace_back("mu","upper");
  if(r.restricted & 16u) restricted.emplace_back("sigma","lower");
  if(r.restricted & 32u) restricted.emplace_back("sigma","upper");
 return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",r.lambda},{"mu",r.mu},{"sigma",r.sigma}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
