#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/pareto/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_pareto(cppunivariate::span x, Held a_arg = {}, Held b_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held a = a_arg;
  const Held b = b_arg;

  std::vector<std::string> fixed;
  const double a_held = !a.supplied ? std::numeric_limits<double>::quiet_NaN() : a.value;
  if (a.supplied) {
    if (!(std::isfinite(a_held) && a_held > 0.0)) return fail("Held `a` must be positive and finite.");
    fixed.push_back("a");
  }
  const double b_held = !b.supplied ? std::numeric_limits<double>::quiet_NaN() : b.value;
  if (b.supplied) {
    if (!(std::isfinite(b_held) && b_held > 0.0)) return fail("Held `b` must be positive and finite.");
    fixed.push_back("b");
  }
  if (b.supplied && std::any_of(x.begin(),x.end(),[&](double v){return v<b_held;})) return fail("Held b exceeds the sample minimum: zero likelihood.");

  auto res = cppunivariate::pareto_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, a_held, b_held);
  if (!(res.shape>0.0) || !std::isfinite(res.shape) || !std::isfinite(res.logLik))
    return fail("The Pareto maximum likelihood estimator does not exist for constant data; other data may exceed double precision.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"a",res.shape},{"b",res.min}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
