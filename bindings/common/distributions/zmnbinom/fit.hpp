#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/zmnbinom/mle.hpp"
namespace umvl_binding {
inline FitResult fit_zmnbinom(cppunivariate::ispan x, Held size_arg = {}, Held prob_arg = {}, Held p0_arg = {}, double reltol = 1e-10, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held size = size_arg;
  const Held prob = prob_arg;
  const Held p0 = p0_arg;

  if(prob.supplied && !std::isfinite(prob.value))return fail("`prob` must be finite and in its parameter domain.");
  if(p0.supplied && !std::isfinite(p0.value))return fail("`p0` must be finite and in its parameter domain.");

  const double fixed_size=size.supplied?size.value:std::numeric_limits<double>::quiet_NaN();
  if(size.supplied && !std::isfinite(fixed_size))return fail("`size` must be finite and in its parameter domain.");
  const auto res=cppunivariate::zmnbinom_mle(x, reltol, iterlim, fixed_size,prob.supplied?prob.value:std::numeric_limits<double>::quiet_NaN(),p0.supplied?p0.value:std::numeric_limits<double>::quiet_NaN());
  if(res.status==cppunivariate::ZmnbinomStatus::InvalidInput)return fail("requires nonempty nonnegative integer count data and valid controls and parameter domains; held values must be compatible with the observations");
  if(res.status==cppunivariate::ZmnbinomStatus::InvalidSize)return fail("`size` must be a nonnegative finite value.");
  if(res.status==cppunivariate::ZmnbinomStatus::PoissonLimit)return fail("The maximum likelihood estimator escapes to infinite size. Use `mlzmpois`.");
  if(res.status==cppunivariate::ZmnbinomStatus::AllZero)warnings.push_back(binding_message("all observations are zero: base parameters are unidentified; supplied values are preserved."));
  if(res.status==cppunivariate::ZmnbinomStatus::TwoPoint)warnings.push_back(binding_message("positives are all one: the attained two-point law is also fitted exactly by mlbinom(x, size = 1)."));
  if(res.status==cppunivariate::ZmnbinomStatus::IterationFailure)return fail("Profile search exhausted floating-point resolution; no capped size estimate is returned.");
  if(!std::isfinite(res.logLik))return fail("MLE cannot be resolved at floating-point precision.");
  std::vector<std::string> fixed;
  if(size.supplied)fixed.push_back("size");
  if(prob.supplied)fixed.push_back("prob");
  if(p0.supplied)fixed.push_back("p0");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",res.size},{"prob",res.prob},{"p0",res.p0}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
