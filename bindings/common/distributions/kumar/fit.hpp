#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/kumar/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_kumar(cppunivariate::span x, Held a_arg = {}, Held b_arg = {}, Held a0 = {}, double reltol = 1.220703e-4, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held a = a_arg;
  const Held b = b_arg;

  double start = std::numeric_limits<double>::quiet_NaN();
  if (a0.supplied) {
    start = a0.value;
    if (!(start > 0.0) || !std::isfinite(start))
      return fail("`a0` must be a positive finite number.");
  }
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double checked_a = a.supplied ? a.value : nan;
  if (a.supplied && (!std::isfinite(checked_a) || (true && !(checked_a > 0)))) return fail("Held parameters must be finite and shapes/scales positive.");
  const double checked_b = b.supplied ? b.value : nan;
  if (b.supplied && (!std::isfinite(checked_b) || (true && !(checked_b > 0)))) return fail("Held parameters must be finite and shapes/scales positive.");



  std::vector<std::string> fixed;
  if (a.supplied) fixed.push_back("a");
  if (b.supplied) fixed.push_back("b");
  auto res = cppunivariate::kumar_mle(x, reltol, iterlim, start, checked_a, checked_b);
  if(res.certificate_failure==cppunivariate::KumarCertificateFailure::Budget)
    return stop_failure(fail("Kumaraswamy MLE failed: iteration certificate budget exhausted (16 * min(iterlim, 100000)); no verified maximum."),res.stop_detail,"budget",{"a","b"});
  if (res.status == cppunivariate::KumarStatus::Unbounded)
    return fail("Kumaraswamy likelihood is unbounded for constant samples.");
  if (res.status != cppunivariate::KumarStatus::Ok || !std::isfinite(res.a) || !std::isfinite(res.b) || !std::isfinite(res.logLik)) {
    return fail("Kumaraswamy MLE failed: iteration limit or shapes cannot be resolved in double precision; increase iterlim.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"a",res.a},{"b",res.b}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }();

}
}
