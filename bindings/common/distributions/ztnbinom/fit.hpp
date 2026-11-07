#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/ztnbinom/mle.hpp"
namespace umvl_binding {
inline FitResult fit_ztnbinom(cppunivariate::ispan x, Held size_arg = {}, Held prob_arg = {}, double reltol = 1e-10, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held size = size_arg;
  const Held prob = prob_arg;

  if(prob.supplied && !std::isfinite(prob.value))return fail("`prob` must be finite and in its parameter domain.");

  const double fixed_size=size.supplied?size.value:std::numeric_limits<double>::quiet_NaN();
  if(size.supplied && !std::isfinite(fixed_size))return fail("`size` must be finite and in its parameter domain.");
  const auto res=cppunivariate::ztnbinom_mle(x, reltol, iterlim, fixed_size,prob.supplied?prob.value:std::numeric_limits<double>::quiet_NaN());
  if(res.status==cppunivariate::ZtnbinomStatus::InvalidInput)return fail("requires nonempty positive integer count data and valid controls and parameter domains; held values must be compatible with the observations");
  if(res.status==cppunivariate::ZtnbinomStatus::InvalidSize)return fail("`size` must be a nonnegative finite value.");
  if(res.status==cppunivariate::ZtnbinomStatus::PoissonLimit)return stop_failure(fail("The maximum likelihood estimator escapes to infinite size. Use `mlztpois`."),res.stop_detail,"limit",{"lambda"},"ztpois");
  if(res.status==cppunivariate::ZtnbinomStatus::IterationFailure) {
    std::string message="Profile certificate has an unresolved floating-point comparison; no verified maximum.";
    if(res.certificate_failure==cppunivariate::ZtnbinomCertificateFailure::Budget)message="Profile search exhausted: certificate iteration budget exhausted (128 * min(iterlim, 100000)); no verified maximum. Raising iterlim may resolve this stop.";
    if(res.certificate_failure==cppunivariate::ZtnbinomCertificateFailure::Workload)message="Profile certificate workload limit: maximum count 100000 and total count 2^52; no verified maximum.";
    if(res.certificate_failure==cppunivariate::ZtnbinomCertificateFailure::AdditionalExtrema)message="Profile certificate found additional extrema requiring a global likelihood comparison; no verified maximum.";
    return stop_failure(fail(message),res.stop_detail,res.budget_exhausted?"budget":"numerical",{"size","prob"});
  }
  if(!std::isfinite(res.logLik))return fail("MLE cannot be resolved at floating-point precision.");
  std::vector<std::string> fixed;
  if(size.supplied)fixed.push_back("size");
  if(prob.supplied)fixed.push_back("prob");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",res.size},{"prob",res.prob}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
