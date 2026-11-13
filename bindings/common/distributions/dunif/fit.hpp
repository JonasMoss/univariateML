#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/dunif/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_dunif(cppunivariate::ispan x, Held min_arg = {}, Held max_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held min = min_arg;
  const Held max = max_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=!min.supplied?nan:min.value, b=!max.supplied?nan:max.value;
  if ((min.supplied&&!std::isfinite(a)) || (max.supplied&&!std::isfinite(b)))
    return invalid_argument(fail("Held endpoints must be finite integers."));
  auto res = cppunivariate::dunif_mle(
      cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())},a,b);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::DiscreteUnifStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
  if (res.status!=cppunivariate::DiscreteUnifStatus::Ok) return done(fail("Held endpoints must be integers covering every observation."));
  std::vector<std::string> fixed;
  if(min.supplied)fixed.push_back("min");
  if(max.supplied)fixed.push_back("max");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"min",res.min},{"max",res.max}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
