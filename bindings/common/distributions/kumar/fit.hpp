#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/kumar/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_kumar(cppunivariate::span x, Held a_arg = {}, Held b_arg = {}, double tol = 1e-10, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held a = a_arg;
  const Held b = b_arg;

  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double checked_a = a.supplied ? a.value : nan;
  if (a.supplied && (!std::isfinite(checked_a) || (true && !(checked_a > 0)))) return invalid_argument(fail("Held parameters must be finite and shapes/scales positive."));
  const double checked_b = b.supplied ? b.value : nan;
  if (b.supplied && (!std::isfinite(checked_b) || (true && !(checked_b > 0)))) return invalid_argument(fail("Held parameters must be finite and shapes/scales positive."));



  std::vector<std::string> fixed;
  if (a.supplied) fixed.push_back("a");
  if (b.supplied) fixed.push_back("b");
  auto res = cppunivariate::kumar_mle(x, tol, iterlim, checked_a, checked_b);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"a",a_arg.supplied},{"b",b_arg.supplied}});
  };
  if (res.status == cppunivariate::KumarStatus::ShapeOverflow)
    return done(fail("Kumaraswamy second shape at the maximum exceeds double precision range."));
  if(res.certificate_failure==cppunivariate::KumarCertificateFailure::Budget)
    return done(stop_failure(fail("Kumaraswamy MLE failed: iteration certificate budget exhausted (128 * min(iterlim, 100000)); no verified maximum. Raising iterlim may resolve this stop."),res.stop_detail,outcome_payload(o),{"a","b"}));
  if (res.status == cppunivariate::KumarStatus::Unbounded)
    return done(fail("Kumaraswamy likelihood is unbounded for constant samples."));
  if (res.status != cppunivariate::KumarStatus::Ok) {
    return done(stop_failure(fail("Kumaraswamy MLE failed: iteration limit or shapes cannot be resolved in double precision; increase iterlim."),res.stop_detail,outcome_payload(o),{"a","b"}));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"a",res.a},{"b",res.b}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }());

}
}
