#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/binom/tbinom_mle.hpp"
#include <algorithm>
#include <sstream>
namespace umvl_binding {
inline FitResult fit_tbinom_controlled(cppunivariate::ispan x, Held prob_arg, Held a_arg, Held b_arg, Held size_arg, double tol,int maxit,bool legacy_default=false) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held prob = prob_arg;
  const Held a = a_arg;
  const Held b = b_arg;
  const Held size = size_arg;

  const double held=!prob.supplied?std::numeric_limits<double>::quiet_NaN():prob.value;
  if(prob.supplied&&std::isnan(held))return invalid_argument(fail("Fixed parameter must not be missing."));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double lower=a.supplied?a.value:x.empty()?nan:double(*std::min_element(x.begin(),x.end()))-1;
  const double upper=b.supplied?b.value:x.empty()?nan:double(*std::max_element(x.begin(),x.end()));
  const double trials=size.supplied?size.value:nan;
  if(!x.empty() && ((a.supplied&&std::isnan(lower))||(b.supplied&&std::isnan(upper))))return invalid_argument(fail("Held bounds must not be missing."));
  if(!x.empty() && size.supplied && !std::isfinite(trials))return invalid_argument(fail("`size` must be a finite nonnegative integer."));
  auto r=legacy_default?cppunivariate::tbinom_design_mle(x,lower,upper,trials,held):
    cppunivariate::tbinom_design_mle(x,lower,upper,trials,held,tol,maxit);
  const auto o=cppunivariate::outcome(r);
  const auto done=[&](FitResult out) {
    return with_outcome(std::move(out),o,static_cast<int>(r.status),{{"prob",prob_arg.supplied},{"a",a_arg.supplied},{"b",b_arg.supplied},{"size",size_arg.supplied}});
  };
  return done([&]() -> FitResult {
  PayloadKind stop_kind=PayloadKind::None;
  switch(r.status) {
    case cppunivariate::TbinomStatus::LowerPointMass: stop_kind=outcome_payload(o);break;
    case cppunivariate::TbinomStatus::UpperPointMass: stop_kind=outcome_payload(o);break;
    case cppunivariate::TbinomStatus::TpoisLimit: stop_kind=outcome_payload(o);break;
    case cppunivariate::TbinomStatus::Unidentified: stop_kind=PayloadKind::Comparison;break;
    case cppunivariate::TbinomStatus::NumericalFailure: stop_kind=outcome_payload(o);break;
    default:break;
  }
  if(r.stop_detail.comparison_tie)stop_kind=PayloadKind::Unresolved;
  if(r.stop_detail.budget_exhausted)stop_kind=PayloadKind::Budget;
  if((stop_kind==PayloadKind::None) && !r.stop_detail.candidates.empty())stop_kind=PayloadKind::Numerical;
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(stop_kind==PayloadKind::Unresolved ? "Global likelihood comparison is unresolved; no maximizer is certified. Use `mlpois(x, truncate = c(max(0, floor(a) + 1), floor(b)))` only to fit the comparison face." : binding_message(format,arguments...));
    if(stop_kind==PayloadKind::None)return out;
    return comparison_payload(stop_failure(out,r.stop_detail,stop_kind,{"prob","a","b","size"}),o,
      cppunivariate::comparison_choice(r),{{"prob",prob_arg},{"a",a_arg},{"b",b_arg},{"size",size_arg}});
  };

  if(r.status==cppunivariate::TbinomStatus::LowerPointMass) return stopped_fail("MLE escapes to a point mass at the lowest support point.");
  if(r.status==cppunivariate::TbinomStatus::UpperPointMass) return stopped_fail("MLE escapes to a point mass at the highest support point.");
  if(r.status==cppunivariate::TbinomStatus::Unidentified) return stopped_fail("Parameter is unidentified on singleton truncation support.");
  if(r.status==cppunivariate::TbinomStatus::InvalidInput && x.empty())return stopped_fail("requires nonempty data");
  if(r.status==cppunivariate::TbinomStatus::InvalidInput) return stopped_fail("Invalid design parameters or observations outside truncation bounds a < x <= b.");
  if(r.status==cppunivariate::TbinomStatus::TpoisLimit) {
    std::ostringstream os;
    if(r.certified)
      os << "The maximum likelihood estimator does not exist: with both `prob` and `size` "
            "free, the global likelihood supremum lies at the truncated Poisson distribution as `size` grows.";
    else
      os << "The size profile favors the truncated Poisson limit within the likelihood comparison margin.";
    os << std::setprecision(17) << " Use `mlpois(x, truncate = c(" << std::max(0.0,std::floor(lower)+1) << ", ";
    if(std::isinf(upper))os << "Inf";else os << std::floor(upper);
    os << "))` instead.";
    return stopped_fail(os.str());
  }
  if(r.status!=cppunivariate::TbinomStatus::Ok) return stopped_fail("Truncated normalizer cannot be resolved within numerical precision or work budget.");
  std::vector<std::string> fixed;
  if(size.supplied)fixed.push_back("size");
  if(a.supplied)fixed.push_back("a");
  if(b.supplied)fixed.push_back("b");
  if(prob.supplied)fixed.push_back("prob");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"prob",r.prob},{"a",lower},{"b",upper},{"size",r.size}};  out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

  }());
}
}
