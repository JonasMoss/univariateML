#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/nbinom/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_nbinom(cppunivariate::ispan x, Held size_arg = {}, Held prob_arg = {}, double reltol = 1.220703e-4, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held size = size_arg;
  const Held prob = prob_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  if ((size.supplied && !std::isfinite(size.value)) ||
      (prob.supplied && !std::isfinite(prob.value)))
    return fail("Held size and prob must be finite.");
  const auto r=cppunivariate::nbinom_mle(x,reltol,iterlim,
      size.supplied?size.value:nan,prob.supplied?prob.value:nan);
  if(r.certificate_failure==cppunivariate::NbinomCertificateFailure::Budget)
    return stop_failure(fail("Negative binomial MLE failed convergence: insufficient precision or certificate budget exhausted (16 * min(iterlim, 100000)); no verified maximum."),r.stop_detail,"budget",{"size","prob"});
  if(r.status==cppunivariate::NbinomStatus::PrecisionFailure)
    return fail(size.supplied && r.certificate_failure==cppunivariate::NbinomCertificateFailure::None ? "Negative binomial MLE probability rounds to one: held size exceeds double precision for these data." : "Negative binomial MLE failed: insufficient precision to resolve the size estimate.");
  if(r.status==cppunivariate::NbinomStatus::Underdispersed)
    return [&]() { auto out=stop_failure(fail("The maximum likelihood estimator does not exist for underdispersed data, but converges to a Poisson. Use `mlpois` instead."),r.stop_detail,"limit",{"lambda"},"pois"); return out; }();
  if(r.status!=cppunivariate::NbinomStatus::Ok || !std::isfinite(r.logLik))
    return fail("Negative binomial MLE failed: invalid or incompatible holds, inputs or convergence.");
  std::vector<std::string> fixed;
  if(size.supplied)fixed.push_back("size");
  if(prob.supplied)fixed.push_back("prob");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",r.size},{"prob",r.prob}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
