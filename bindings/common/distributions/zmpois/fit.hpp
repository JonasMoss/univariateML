#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/zmpois/mle.hpp"
namespace umvl_binding {
inline FitResult fit_zmpois(cppunivariate::ispan x, Held lambda_arg = {}, Held p0_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held lambda = lambda_arg;
  const Held p0 = p0_arg;

  if(lambda.supplied && !std::isfinite(lambda.value))return fail("`lambda` must be finite and in its parameter domain.");
  if(p0.supplied && !std::isfinite(p0.value))return fail("`p0` must be finite and in its parameter domain.");

  const auto res=cppunivariate::zmpois_mle(x,1e-10,200,lambda.supplied?lambda.value:std::numeric_limits<double>::quiet_NaN(),p0.supplied?p0.value:std::numeric_limits<double>::quiet_NaN());
  if(res.status==cppunivariate::ZmpoisStatus::InvalidInput)return fail("requires nonempty nonnegative integer count data and valid controls and parameter domains; held values must be compatible with the observations");
  if(res.status==cppunivariate::ZmpoisStatus::AllZero)warnings.push_back(binding_message("all observations are zero: base parameters are unidentified; supplied values are preserved."));
  if(res.status==cppunivariate::ZmpoisStatus::TwoPoint)warnings.push_back(binding_message("positives are all one: the attained two-point law is also fitted exactly by mlbinom(x, size = 1)."));
  if(!std::isfinite(res.logLik))return fail("MLE cannot be resolved at floating-point precision.");
  std::vector<std::string> fixed;
  if(lambda.supplied)fixed.push_back("lambda");
  if(p0.supplied)fixed.push_back("p0");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",res.lambda},{"p0",res.p0}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
