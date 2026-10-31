#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/tlambda/mle.hpp"
namespace umvl_binding {
inline FitResult fit_tlambda(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held lambda_arg = {}, double reltol=1e-7, int iterlim=1000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;
  const Held lambda = lambda_arg;

  const double l=lambda.supplied?lambda.value:std::numeric_limits<double>::quiet_NaN();
  if(lambda.supplied && !std::isfinite(l)) return fail("Fixed lambda must be finite.");
  const double m=mu.supplied?mu.value:std::numeric_limits<double>::quiet_NaN();
  const double s=sigma.supplied?sigma.value:std::numeric_limits<double>::quiet_NaN();
  if(mu.supplied&&!std::isfinite(m))return fail("Held mu must be finite.");
  if(sigma.supplied&&!(std::isfinite(s)&&s>0))return fail("Held sigma must be positive and finite.");
  const auto f=cppunivariate::tlambda_mle(x,reltol,iterlim,l,m,s);
  if(f.status==cppunivariate::TlambdaStatus::Unbounded)return fail("Tukey lambda likelihood is unbounded through scale collapse at the held location.");
  if(f.status==cppunivariate::TlambdaStatus::CollapseLimit)return fail("Tukey lambda selects the scale collapse boundary at the held location.");
  if(f.status==cppunivariate::TlambdaStatus::InvalidSupport)return fail("Held Tukey lambda support cannot contain the sample with positive likelihood.");
  if(f.status==cppunivariate::TlambdaStatus::InvalidShape) return fail("Fixed lambda must be finite.");
  if(f.status==cppunivariate::TlambdaStatus::InvalidData) return fail("Tukey lambda fitting requires at least three finite observations, every tie multiplicity strictly below half the sample size, positive reltol and positive iterlim. The sample range must be finite and standardization must preserve distinct values.");
  if(f.status!=cppunivariate::TlambdaStatus::Ok) return fail("Tukey lambda search did not converge; increase iterlim.");
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");
  if(lambda.supplied)fixed.push_back("lambda");
  std::vector<std::pair<std::string,std::string>> restricted;
  if(f.restricted & 1u) restricted.emplace_back("mu","lower");
  if(f.restricted & 2u) restricted.emplace_back("mu","upper");
  if(f.restricted & 4u) restricted.emplace_back("sigma","lower");
  if(f.restricted & 8u) restricted.emplace_back("sigma","upper");
  if(f.restricted & 16u) restricted.emplace_back("lambda","lower");
  if(f.restricted & 32u) restricted.emplace_back("lambda","upper");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",f.mu},{"sigma",f.sigma},{"lambda",f.lambda}}; out.logLik = f.logLik; out.iterations = f.iterations; out.has_iterations = true; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
