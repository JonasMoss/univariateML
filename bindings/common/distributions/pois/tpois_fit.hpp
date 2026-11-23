#pragma once
#include "../../count_maximizers.hpp"
#include "cppunivariate/distributions/pois/tpois_mle.hpp"
namespace umvl_binding {
inline void attach_tpois_window_set(FitResult& out, double lo) {
  if (out.outcome.kind != cppunivariate::OutcomeKind::Estimate ||
      out.outcome.unique || !out.outcome.uniqueness_known) return;
  const bool include_design = out.estimates.size() == 3;
  if (include_design && keep_infinite_window_comparison(out)) return;

  // The singleton retains every positive rate, and the rate-zero atom
  // exactly when the count is zero.
  const auto rate = cppunivariate::interval(0, INFINITY, lo <= 0, false);
  cppunivariate::MaximizerSet set;
  set.dimension = out.estimates.size();
  if (include_design)
    set.add_box({{rate, cppunivariate::fixed(out.estimates[1].second),
                        cppunivariate::fixed(out.estimates[2].second)}});
  else
    set.add_box({{rate}});
  attach_count_set(out, out.outcome, set, "tpois", {});
}

inline FitResult fit_tpois(cppunivariate::ispan x, Held lambda_arg = {}, Held a_arg = {}, Held b_arg = {}, double tol=1e-10,int maxit=200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held lambda = lambda_arg;
  const Held a = a_arg;
  const Held b = b_arg;

  const double held=!lambda.supplied?std::numeric_limits<double>::quiet_NaN():lambda.value;
  if(lambda.supplied&&std::isnan(held))return invalid_argument(fail("Fixed parameter must not be missing."));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  double lower=!a.supplied?nan:a.value, upper=!b.supplied?nan:b.value;
  if((a.supplied&&std::isnan(lower))||(b.supplied&&std::isnan(upper)))
    return invalid_argument(fail("Held bounds must not be missing."));
  const auto r=cppunivariate::tpois_mle(x,lower,upper,held,tol,maxit);
  const auto o=cppunivariate::outcome(r);
  const auto done=[&](FitResult out) {
    return with_outcome(std::move(out),o,static_cast<int>(r.status),{{"lambda",lambda_arg.supplied},{"a",a_arg.supplied},{"b",b_arg.supplied}});
  };
  return done([&]() -> FitResult {
  PayloadKind stop_kind=PayloadKind::None;
  switch(r.status) {
    case cppunivariate::truncated_detail::Status::LowerPointMass: stop_kind=outcome_payload(o);break;
    case cppunivariate::truncated_detail::Status::UpperPointMass: stop_kind=outcome_payload(o);break;
    case cppunivariate::truncated_detail::Status::Unidentified: stop_kind=PayloadKind::Comparison;break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    if(stop_kind==PayloadKind::None)return out;
    return comparison_payload(stop_failure(out,r.stop_detail,stop_kind,{"lambda","a","b"}),o,
      cppunivariate::comparison_choice(r),{{"lambda",lambda_arg},{"a",a_arg},{"b",b_arg}});
  };

  if(x.size()>0) {
    if(!a.supplied)lower=static_cast<double>(*std::min_element(x.begin(),x.end()))-1;
    if(!b.supplied)upper=*std::max_element(x.begin(),x.end());
  }
  if(r.status==cppunivariate::truncated_detail::Status::LowerPointMass) return stopped_fail("MLE escapes to a point mass at the lowest support point.");
  if(r.status==cppunivariate::truncated_detail::Status::UpperPointMass) return stopped_fail("MLE escapes to a point mass at the highest support point.");
  if(r.status==cppunivariate::truncated_detail::Status::Unidentified) return stopped_fail("Parameter is unidentified on singleton truncation support.");
  if(r.status==cppunivariate::truncated_detail::Status::InvalidInput) return stopped_fail("Invalid design parameters or observations outside truncation bounds a < x <= b.");
  if(r.status!=cppunivariate::truncated_detail::Status::Ok) return stopped_fail("Truncated normalizer cannot be resolved within numerical precision or work budget.");
  std::vector<std::string> fixed;
  if(a.supplied)fixed.push_back("a");
  if(b.supplied)fixed.push_back("b");
  if(lambda.supplied)fixed.push_back("lambda");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",r.parameter},{"a",lower},{"b",upper}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

  }());
}
}
