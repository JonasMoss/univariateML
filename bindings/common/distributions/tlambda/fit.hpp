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
  if(lambda.supplied && !std::isfinite(l)) return invalid_argument(fail("Fixed lambda must be finite."));
  const double m=mu.supplied?mu.value:std::numeric_limits<double>::quiet_NaN();
  const double s=sigma.supplied?sigma.value:std::numeric_limits<double>::quiet_NaN();
  if(mu.supplied&&!std::isfinite(m))return invalid_argument(fail("Held mu must be finite."));
  if(sigma.supplied&&!(std::isfinite(s)&&s>0))return invalid_argument(fail("Held sigma must be positive and finite."));
  const auto f=cppunivariate::tlambda_mle(x,reltol,iterlim,l,m,s);
  const auto o = cppunivariate::outcome(f);
  const auto done = [&](FitResult out) {
    if (f.status == cppunivariate::TlambdaStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(f.status));
    return with_outcome(std::move(out), o, static_cast<int>(f.status),{{"mu",mu_arg.supplied},{"sigma",sigma_arg.supplied},{"lambda",lambda_arg.supplied}});
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };
  if(f.status==cppunivariate::TlambdaStatus::CertificateFailure) {
    const std::vector<std::string> names{"mu","sigma","lambda"};
    if(f.stop_detail.budget_exhausted)
      return done(stop_failure(fail("Tukey lambda certificate budget exhausted; increase iterlim."),f.stop_detail,outcome_payload(o),names));
    if(f.stop_detail.comparison_tie)
      return done(comparison_payload(stop_failure(fail("Tukey lambda support-contact maxima are tied or numerically indistinguishable."),f.stop_detail,PayloadKind::Comparison,names),o,cppunivariate::comparison_choice(f),{{"mu",mu_arg},{"sigma",sigma_arg},{"lambda",lambda_arg}}));
    if(!lambda.supplied && !mu.supplied && !sigma.supplied && !f.stop_detail.limit_coef.empty())
      return done(stop_failure(fail("Tukey lambda tie multiplicity is half the sample size; the free-shape global/collapse comparison is unresolved."),f.stop_detail,outcome_payload(o),names));
    return done(stop_failure(fail("Tukey lambda certificate did not converge: numerical failure (inverse enclosure, arithmetic or unresolved global/collapse comparison)."),f.stop_detail,outcome_payload(o),names));
  }
  if(f.status==cppunivariate::TlambdaStatus::TransformFailure)return done(fail("Tukey lambda standardization or arithmetic cannot represent this valid sample."));
  if(f.status==cppunivariate::TlambdaStatus::Unbounded)return done(nomle("Tukey lambda likelihood is unbounded through scale collapse."));
  if(f.status==cppunivariate::TlambdaStatus::CollapseLimit)return done(stop_failure(fail("Tukey lambda selects the unattained scale collapse boundary."),f.stop_detail,outcome_payload(o),{"mu","sigma","lambda"},"unavailable:tlambda-scale-collapse"));
  if(f.status==cppunivariate::TlambdaStatus::InvalidSupport)return done(fail("Held Tukey lambda support cannot contain the sample with positive likelihood."));
  if(f.status==cppunivariate::TlambdaStatus::InvalidShape) return done(fail("Fixed lambda must be finite."));
  if(f.status==cppunivariate::TlambdaStatus::InvalidData)return done(fail("Tukey lambda fitting requires finite observations, positive reltol and positive iterlim; numerical standardization must preserve distinct values."));
  if(f.status!=cppunivariate::TlambdaStatus::Ok) return done(stop_failure(fail("Tukey lambda search did not converge; increase iterlim."),f.stop_detail,outcome_payload(o),{"mu","sigma","lambda"}));
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
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",f.mu},{"sigma",f.sigma},{"lambda",f.lambda}}; out.logLik = f.logLik; out.iterations = f.iterations; out.has_iterations = true; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
