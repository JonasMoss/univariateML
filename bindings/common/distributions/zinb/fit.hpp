#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/zinb/mle.hpp"
namespace umvl_binding {
inline FitResult fit_zinb(cppunivariate::ispan x, Held size_arg = {}, Held prob_arg = {}, Held pi_arg = {}, double reltol = 1e-10, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held size = size_arg;
  const Held prob = prob_arg;
  const Held pi = pi_arg;

  if(prob.supplied && !std::isfinite(prob.value))return fail("`prob` must be finite and in its parameter domain.");
  if(pi.supplied && !std::isfinite(pi.value))return fail("`pi` must be finite and in its parameter domain.");

  if (size.supplied && (!std::isfinite(size.value) || size.value < 0))
    return fail("size must be a finite nonnegative number");
  const auto fit = cppunivariate::zinb_mle(x, reltol, iterlim,
      size.supplied ? size.value : NAN,prob.supplied?prob.value:std::numeric_limits<double>::quiet_NaN(),pi.supplied?pi.value:std::numeric_limits<double>::quiet_NaN());
  if (fit.status == cppunivariate::ZinbStatus::InvalidSize)
    return fail("size must be a finite nonnegative number");
  if (fit.status == cppunivariate::ZinbStatus::InvalidInput)
    return fail("ZINB requires nonempty nonnegative count data, positive reltol and iterlim, prob in (0,1], pi in [0,1], and compatible holds");
  if (fit.status == cppunivariate::ZinbStatus::ZipLimit)
    return fail("The maximum likelihood estimator does not exist, but converges to a zero-inflated Poisson. Use `mlzip` instead.");
  if (fit.status == cppunivariate::ZinbStatus::PoissonLimit)
    return fail("The maximum likelihood estimator does not exist, but converges to a Poisson. Use `mlpois` instead.");
  if (fit.status != cppunivariate::ZinbStatus::Ok)
    return fail("Zero-inflated negative binomial MLE failed");
  std::vector<std::string> fixed;
  if(size.supplied)fixed.push_back("size");
  if(prob.supplied)fixed.push_back("prob");
  if(pi.supplied)fixed.push_back("pi");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",fit.size},{"prob",fit.prob},{"pi",fit.pi}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
