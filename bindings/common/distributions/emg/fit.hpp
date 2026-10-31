#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/emg/mle.hpp"
namespace umvl_binding {
inline FitResult fit_emg(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held lambda_arg = {}, double reltol=1e-8, int iterlim=500) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;
  const Held lambda = lambda_arg;

    double checked_value_0 = std::numeric_limits<double>::quiet_NaN();
  if (mu.supplied) {
const Held a = mu;

    const double v=a.value;
    if(!std::isfinite(v))return fail("Held parameters must be finite.");
    checked_value_0 = v;

  }
  double checked_value_1 = std::numeric_limits<double>::quiet_NaN();
  if (sigma.supplied) {
const Held a = sigma;

    const double v=a.value;
    if(!std::isfinite(v))return fail("Held parameters must be finite.");
    checked_value_1 = v;

  }
  double checked_value_2 = std::numeric_limits<double>::quiet_NaN();
  if (lambda.supplied) {
const Held a = lambda;

    const double v=a.value;
    if(!std::isfinite(v))return fail("Held parameters must be finite.");
    checked_value_2 = v;

  }

  auto r=cppunivariate::emg_mle(x,reltol,iterlim,checked_value_0,checked_value_1,checked_value_2);
  if(r.status==cppunivariate::EmgStatus::NormalLimit)return fail("The EMG MLE does not exist: normal face; use mlnorm for an unrestricted normal fit. The compared face retains held mu/sigma.");
  if(r.status==cppunivariate::EmgStatus::TranslatedExponentialLimit)return fail("The EMG MLE does not exist: translated exponential face. No translated exponential fitter is available; for a free endpoint fit distances with mlexp(x - min(x)).");
  if(r.status==cppunivariate::EmgStatus::Unbounded)return fail("The EMG likelihood is unbounded for these constant observations.");
  if(r.status==cppunivariate::EmgStatus::InvalidInput)return fail("EMG requires finite data, finite mu, positive sigma and lambda, positive reltol and iterlim.");
  if(r.status==cppunivariate::EmgStatus::IterationFailure)return fail("EMG search did not converge; increase iterlim.");
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");
  if(lambda.supplied)fixed.push_back("lambda");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma},{"lambda",r.lambda}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
