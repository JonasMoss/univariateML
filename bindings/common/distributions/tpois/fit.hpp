#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/tpois/mle.hpp"
namespace umvl_binding {
inline FitResult fit_tpois(cppunivariate::ispan x, Held lambda_arg = {}, Held a_arg = {}, Held b_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held lambda = lambda_arg;
  const Held a = a_arg;
  const Held b = b_arg;

  const double held=!lambda.supplied?std::numeric_limits<double>::quiet_NaN():lambda.value;
  if(lambda.supplied&&std::isnan(held))return fail("Fixed parameter must not be missing.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  double lower=!a.supplied?nan:a.value, upper=!b.supplied?nan:b.value;
  if((a.supplied&&std::isnan(lower))||(b.supplied&&std::isnan(upper)))
    return fail("Held bounds must not be missing.");
  auto r=cppunivariate::tpois_mle(x,lower,upper,held);
  if(x.size()>0) {
    if(!a.supplied)lower=static_cast<double>(*std::min_element(x.begin(),x.end()))-1;
    if(!b.supplied)upper=*std::max_element(x.begin(),x.end());
  }
  if(r.status==cppunivariate::truncated_detail::Status::LowerPointMass) return fail("MLE escapes to a point mass at the lowest support point.");
  if(r.status==cppunivariate::truncated_detail::Status::UpperPointMass) return fail("MLE escapes to a point mass at the highest support point.");
  if(r.status==cppunivariate::truncated_detail::Status::Unidentified) return fail("Parameter is unidentified on singleton truncation support.");
  if(r.status==cppunivariate::truncated_detail::Status::InvalidInput) return fail("Invalid design parameters or observations outside truncation bounds a < x <= b.");
  if(r.status!=cppunivariate::truncated_detail::Status::Ok) return fail("Truncated normalizer cannot be resolved within numerical precision or work budget.");
  std::vector<std::string> fixed;
  if(a.supplied)fixed.push_back("a");
  if(b.supplied)fixed.push_back("b");
  if(lambda.supplied)fixed.push_back("lambda");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",r.parameter},{"a",lower},{"b",upper}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
