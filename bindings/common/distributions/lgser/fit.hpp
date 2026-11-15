#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/lgser/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_lgser(cppunivariate::ispan x, Held theta_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held theta = theta_arg;

  if(theta.supplied && !std::isfinite(theta.value))return invalid_argument(fail("`theta` must be finite and in its parameter domain."));

  auto res = cppunivariate::lgser_mle(x,theta.supplied?theta.value:std::numeric_limits<double>::quiet_NaN());
  const auto o=cppunivariate::outcome(res);
  const auto done=[&](FitResult out) {
    return with_outcome(std::move(out),o,static_cast<int>(res.status),{{"theta",theta_arg.supplied}});
  };
  return done([&]() -> FitResult {
  PayloadKind stop_kind=PayloadKind::None;
  switch(res.status) {
    case cppunivariate::LgserStatus::PointMass: stop_kind=outcome_payload(o);break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return (stop_kind==PayloadKind::None)?out:stop_failure(out,res.stop_detail,stop_kind,{"theta"});
  };

  if (res.status == cppunivariate::LgserStatus::PointMass) {
    return stopped_fail("All observations equals 1; the maximum likelihood estimator "
               "does not exist.");
  }
  if (res.status == cppunivariate::LgserStatus::InvalidInput) return stopped_fail("Logarithmic-series requires nonempty positive integer data and theta strictly between zero and one.");
  if (res.status != cppunivariate::LgserStatus::Ok) {
    return stopped_fail("Logarithmic-series MLE failed");
  }
  std::vector<std::string> fixed;if(theta.supplied)fixed.push_back("theta");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"theta",res.theta}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true; return out; }();

  }());
}
}
