#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <algorithm>
#include <sstream>
namespace umvl_binding {
inline FitResult fit_tbinom(cppunivariate::ispan x, Held prob_arg = {}, Held a_arg = {}, Held b_arg = {}, Held size_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held prob = prob_arg;
  const Held a = a_arg;
  const Held b = b_arg;
  const Held size = size_arg;

  const double held=!prob.supplied?std::numeric_limits<double>::quiet_NaN():prob.value;
  if(prob.supplied&&std::isnan(held))return fail("Fixed parameter must not be missing.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0)return fail("requires nonempty data");
  const double lower=a.supplied?a.value:double(*std::min_element(x.begin(),x.end()))-1;
  const double upper=b.supplied?b.value:double(*std::max_element(x.begin(),x.end()));
  const double trials=size.supplied?size.value:nan;
  if((a.supplied&&std::isnan(lower))||(b.supplied&&std::isnan(upper)))return fail("Held bounds must not be missing.");
  if(size.supplied && !std::isfinite(trials))return fail("`size` must be a finite nonnegative integer.");
  auto r=cppunivariate::tbinom_design_mle(x,lower,upper,trials,held);
  if(r.status==cppunivariate::TbinomStatus::LowerPointMass) return fail("MLE escapes to a point mass at the lowest support point.");
  if(r.status==cppunivariate::TbinomStatus::UpperPointMass) return fail("MLE escapes to a point mass at the highest support point.");
  if(r.status==cppunivariate::TbinomStatus::Unidentified) return fail("Parameter is unidentified on singleton truncation support.");
  if(r.status==cppunivariate::TbinomStatus::InvalidInput) return fail("Invalid design parameters or observations outside truncation bounds a < x <= b.");
  if(r.status==cppunivariate::TbinomStatus::TpoisLimit) {
    std::ostringstream os;
    os << "The maximum likelihood estimator does not exist: with both `prob` and `size` "
          "free, the likelihood increases toward the truncated Poisson distribution as "
          "`size` grows. Use `mltpois(x, a = " << lower << ", b = " << upper << ")` instead.";
    return fail(os.str());
  }
  if(r.status!=cppunivariate::TbinomStatus::Ok) return fail("Truncated normalizer cannot be resolved within numerical precision or work budget.");
  std::vector<std::string> fixed;
  if(size.supplied)fixed.push_back("size");
  if(a.supplied)fixed.push_back("a");
  if(b.supplied)fixed.push_back("b");
  if(prob.supplied)fixed.push_back("prob");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"prob",r.prob},{"a",lower},{"b",upper},{"size",r.size}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
