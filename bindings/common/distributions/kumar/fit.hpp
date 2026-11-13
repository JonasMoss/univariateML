#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/kumar/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_kumar(cppunivariate::span x, Held a_arg = {}, Held b_arg = {}, Held a0 = {}, double reltol = 1e-10, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held a = a_arg;
  const Held b = b_arg;

  double start = std::numeric_limits<double>::quiet_NaN();
  if (a0.supplied) {
    start = a0.value;
    if (!(start > 0.0) || !std::isfinite(start))
      return invalid_argument(fail("`a0` must be a positive finite number."));
  }
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double checked_a = a.supplied ? a.value : nan;
  if (a.supplied && (!std::isfinite(checked_a) || (true && !(checked_a > 0)))) return invalid_argument(fail("Held parameters must be finite and shapes/scales positive."));
  const double checked_b = b.supplied ? b.value : nan;
  if (b.supplied && (!std::isfinite(checked_b) || (true && !(checked_b > 0)))) return invalid_argument(fail("Held parameters must be finite and shapes/scales positive."));



  std::vector<std::string> fixed;
  if (a.supplied) fixed.push_back("a");
  if (b.supplied) fixed.push_back("b");
  auto res = cppunivariate::kumar_mle(x, reltol, iterlim, start, checked_a, checked_b);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (res.status == cppunivariate::KumarStatus::Ok)
        return overridden(std::move(out), static_cast<int>(res.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(res.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
  if(res.certificate_failure==cppunivariate::KumarCertificateFailure::Budget)
    return done(stop_failure(fail("Kumaraswamy MLE failed: iteration certificate budget exhausted (128 * min(iterlim, 100000)); no verified maximum. Raising iterlim may resolve this stop."),res.stop_detail,outcome_kind(o),{"a","b"}));
  if (res.status == cppunivariate::KumarStatus::Unbounded)
    return done(fail("Kumaraswamy likelihood is unbounded for constant samples."));
  if (res.status != cppunivariate::KumarStatus::Ok || !std::isfinite(res.a) || !std::isfinite(res.b) || !std::isfinite(res.logLik)) {
    return done(fail("Kumaraswamy MLE failed: iteration limit or shapes cannot be resolved in double precision; increase iterlim."));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"a",res.a},{"b",res.b}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }());

}
}
