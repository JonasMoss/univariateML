#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/pois/zip_mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_zip(cppunivariate::ispan x, Held lambda_arg = {}, Held pi_arg = {}, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  if(iterlim<=0)return fail("iterlim must be positive.");
  const Held lambda = lambda_arg;
  const Held pi = pi_arg;

  if(lambda.supplied && !std::isfinite(lambda.value))return fail("`lambda` must be finite and in its parameter domain.");
  if(pi.supplied && !std::isfinite(pi.value))return fail("`pi` must be finite and in its parameter domain.");

  if (x.size() > 0 &&
      std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    warnings.push_back(binding_message("all observations are zero"));
  }
  auto res = cppunivariate::zip_mle(x,lambda.supplied?lambda.value:std::numeric_limits<double>::quiet_NaN(),pi.supplied?pi.value:std::numeric_limits<double>::quiet_NaN(),iterlim);
  std::string stop_kind;
  switch(res.status) {
    case cppunivariate::ZipStatus::Budget: stop_kind="budget";break;
    case cppunivariate::ZipStatus::Tie: stop_kind="tie";break;
    case cppunivariate::ZipStatus::NumericalFailure: stop_kind="numerical";break;
    case cppunivariate::ZipStatus::ZeroLimit: stop_kind="limit";break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,res.stop_detail,stop_kind,{"lambda","pi"});
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
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",res.lambda},{"pi",res.pi}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
