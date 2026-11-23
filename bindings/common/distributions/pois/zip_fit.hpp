#pragma once
#include "../../count_maximizers.hpp"
#include "cppunivariate/distributions/pois/zip_mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_zip(cppunivariate::ispan x, Held lambda_arg = {}, Held pi_arg = {}, int iterlim = 200,double tol=1e-10) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  if(iterlim<=0)return invalid_argument(fail("iterlim must be positive."));
  const Held lambda = lambda_arg;
  const Held pi = pi_arg;

  if(lambda.supplied && !std::isfinite(lambda.value))return invalid_argument(fail("`lambda` must be finite and in its parameter domain."));
  if(pi.supplied && !std::isfinite(pi.value))return invalid_argument(fail("`pi` must be finite and in its parameter domain."));

  auto res = cppunivariate::zip_mle(x,lambda.supplied?lambda.value:std::numeric_limits<double>::quiet_NaN(),pi.supplied?pi.value:std::numeric_limits<double>::quiet_NaN(),iterlim,tol);
  if(res.all_zero)warnings.push_back(binding_message("all observations are zero"));
  auto o =cppunivariate::outcome(res);
  const auto done=[&](FitResult out) {
    return with_outcome(std::move(out),o,static_cast<int>(res.status),{{"lambda",lambda_arg.supplied},{"pi",pi_arg.supplied}});
  };
  return done([&]() -> FitResult {
  PayloadKind stop_kind=PayloadKind::None;
  switch(res.status) {
    case cppunivariate::ZipStatus::Budget: stop_kind=outcome_payload(o);break;
    case cppunivariate::ZipStatus::Tie: stop_kind=PayloadKind::Comparison;break;
    case cppunivariate::ZipStatus::NumericalFailure: stop_kind=outcome_payload(o);break;
    case cppunivariate::ZipStatus::ZeroLimit: stop_kind=outcome_payload(o);break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    if(stop_kind==PayloadKind::None)return out;
    return comparison_payload(stop_failure(out,res.stop_detail,stop_kind,{"lambda","pi"}),o,
      cppunivariate::comparison_choice(res),{{"lambda",lambda_arg},{"pi",pi_arg}});
  };

  if(res.status==cppunivariate::ZipStatus::ZeroLimit)return stopped_fail("With pi held below one and all-zero data, lambda tends to zero, undefined by the reference; use `mlpois`.");
  if(res.status==cppunivariate::ZipStatus::Budget)return stopped_fail("Zero-inflated Poisson rate certificate exhausted its budget. Raising iterlim may resolve this stop.");
  if(res.status==cppunivariate::ZipStatus::Tie)return stopped_fail("Zero-inflated Poisson competing rate maxima overlap at the comparison margin.");
  if (res.status == cppunivariate::ZipStatus::InvalidInput)
    return stopped_fail("Zero-inflated Poisson MLE requires nonempty nonnegative count data, lambda positive and finite, pi in [0,1], and compatible holds.");
  if (res.status != cppunivariate::ZipStatus::Ok)
    return stopped_fail("Zero-inflated Poisson MLE failed to produce a finite valid fit.");
  std::vector<std::string> fixed;
  if(lambda.supplied)fixed.push_back("lambda");
  if(pi.supplied)fixed.push_back("pi");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",res.lambda},{"pi",res.pi}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true;
    const bool all_zero = std::all_of(x.begin(), x.end(), [](int v) { return v == 0; });
    if (all_zero && res.pi == 1) {
      // The mixture is a zero atom exactly when inflation is one or the parent is a zero atom.
      cppunivariate::MaximizerSet set;
      set.dimension = 2;
      const auto positive = cppunivariate::interval(0, INFINITY, false, false);
      set.add_box({{positive, cppunivariate::fixed(1)}});
      attach_count_set(out, o, set, "zip", {lambda, pi});
    }
    return out; }();

  }());
}
}
