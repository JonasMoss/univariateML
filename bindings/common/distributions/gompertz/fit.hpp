#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gompertz/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_gompertz(cppunivariate::span x, Held a_arg = {}, Held b_arg = {}, double reltol = 1.220703e-4, int iterlim = 100) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held a = a_arg;
  const Held b = b_arg;

  std::vector<std::string> fixed;
  const double a_held = !a.supplied ? std::numeric_limits<double>::quiet_NaN() : a.value;
  if (a.supplied) {
    if (!(std::isfinite(a_held) && a_held > 0.0)) return invalid_argument(fail("Held `a` must be positive and finite."));
    fixed.push_back("a");
  }
  const double b_held = !b.supplied ? std::numeric_limits<double>::quiet_NaN() : b.value;
  if (b.supplied) {
    if (!(std::isfinite(b_held) && b_held > 0.0)) return invalid_argument(fail("Held `b` must be positive and finite."));
    fixed.push_back("b");
  }

  auto res = cppunivariate::gompertz_mle(x, reltol, iterlim, a_held, b_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"a",a_arg.supplied},{"b",b_arg.supplied}});
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };

  if(res.status==cppunivariate::GompertzStatus::Unbounded)
    return done(nomle("The Gompertz maximum likelihood estimator does not exist for constant data: the likelihood is unbounded."));

  if (res.status == cppunivariate::GompertzStatus::Overdispersed) {
    return done(stop_failure(fail("The Gompertz maximum likelihood estimator escapes to an exponential "
               "when the score at b=0 is nonpositive. Use `mlexp` instead."),res.stop_detail,outcome_payload(o),{"rate"},"exp"));
  }
  if (res.status != cppunivariate::GompertzStatus::Ok || !std::isfinite(res.a) ||
      !std::isfinite(res.b)) {
    return done(fail("Gompertz MLE failed to converge or data exceed double precision."));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"a",res.a},{"b",res.b}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
