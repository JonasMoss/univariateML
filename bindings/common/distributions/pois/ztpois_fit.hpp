#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/pois/ztpois_mle.hpp"
namespace umvl_binding {
inline FitResult fit_ztpois(cppunivariate::ispan x, Held lambda_arg = {},double tol=1e-10,int maxit=200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held lambda = lambda_arg;

  if(lambda.supplied && !std::isfinite(lambda.value))return invalid_argument(fail("`lambda` must be finite and in its parameter domain."));

  const auto res=cppunivariate::ztpois_mle(x,lambda.supplied?lambda.value:std::numeric_limits<double>::quiet_NaN(),tol,maxit);
  const auto o=cppunivariate::outcome(res);
  const auto done=[&](FitResult out) {
    return with_outcome(std::move(out),o,static_cast<int>(res.status),{{"lambda",lambda_arg.supplied}});
  };
  return done([&]() -> FitResult {
  if(res.status==cppunivariate::ZtpoisStatus::InvalidInput)return fail("requires nonempty positive integer count data, a held coefficient in its domain, and compatible observations");
  if(o.kind!=cppunivariate::OutcomeKind::Estimate)return stop_failure(fail("MLE cannot be resolved at floating-point precision."),res.stop_detail,outcome_payload(o),{"lambda"});
  std::vector<std::string> fixed;if(lambda.supplied)fixed.push_back("lambda");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",res.lambda}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

  }());
}
}
