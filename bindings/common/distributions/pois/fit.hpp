#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/pois/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_pois(cppunivariate::ispan x, Held lambda_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held lambda = lambda_arg;

  if(lambda.supplied && !std::isfinite(lambda.value))return invalid_argument(fail("`lambda` must be finite and in its parameter domain."));

  auto res = cppunivariate::pois_mle(
      cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, lambda.supplied?lambda.value:std::numeric_limits<double>::quiet_NaN());
  const auto o=cppunivariate::outcome(res);
  const auto done=[&](FitResult out) {
    if(!out.error.empty() && out.error_kind!="tie" && o.kind==cppunivariate::OutcomeKind::Estimate)
      return overridden(std::move(out),static_cast<int>(res.status));
    return with_outcome(std::move(out),o,static_cast<int>(res.status));
  };
  return done([&]() -> FitResult {
  if(res.status != cppunivariate::PoisStatus::Ok || !std::isfinite(res.logLik))return fail("Held lambda is outside its domain or incompatible with the observations.");
  std::vector<std::string> fixed;if(lambda.supplied)fixed.push_back("lambda");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",res.lambda}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true; return out; }();

  }());
}
}
