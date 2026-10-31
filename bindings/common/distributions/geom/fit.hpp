#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/geom/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_geom(cppunivariate::ispan x, Held prob_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held prob = prob_arg;

  if(prob.supplied && !std::isfinite(prob.value))return fail("`prob` must be finite and in its parameter domain.");

  auto res = cppunivariate::geom_mle(
      cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, prob.supplied?prob.value:std::numeric_limits<double>::quiet_NaN());
  if(!std::isfinite(res.logLik))return fail("Held prob is outside its domain or incompatible with the observations.");
  std::vector<std::string> fixed;if(prob.supplied)fixed.push_back("prob");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"prob",res.prob}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
