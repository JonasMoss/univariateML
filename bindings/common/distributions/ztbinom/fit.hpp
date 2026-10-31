#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/ztbinom/mle.hpp"
namespace umvl_binding {
inline FitResult fit_ztbinom(cppunivariate::ispan x, Held size_arg = {}, Held prob_arg = {}, double reltol = 1e-10, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held size = size_arg;
  const Held prob = prob_arg;

  const double fixed_size=size.supplied?size.value:std::numeric_limits<double>::quiet_NaN();
  if(size.supplied && !std::isfinite(fixed_size))return fail("`size` must be finite and in its parameter domain.");
  const double held_prob=prob.supplied?prob.value:std::numeric_limits<double>::quiet_NaN();
  if(prob.supplied && (!std::isfinite(held_prob)||held_prob<0||held_prob>1))return fail("`prob` must be finite and in [0, 1].");
  const auto res=cppunivariate::ztbinom_mle(x, reltol, iterlim, fixed_size, held_prob);
  if(res.status==cppunivariate::ZtbinomStatus::InvalidInput)return fail("requires nonempty positive integer count data and valid controls; held parameters must give positive likelihood");
  if(res.status==cppunivariate::ZtbinomStatus::InvalidSize)return fail("`size` must be a positive integer at least max(x).");
  if(res.status==cppunivariate::ZtbinomStatus::PoissonLimit)return fail("The maximum likelihood estimator escapes to infinite size. Use `mlztpois`.");
  if(res.status==cppunivariate::ZtbinomStatus::IterationFailure)return fail("Profile optimizer IterationFailure: iteration limit or floating-point resolution exhausted; increase iterlim. No capped size estimate is returned.");
  if(!std::isfinite(res.logLik))return fail("MLE cannot be resolved at floating-point precision.");
  std::vector<std::string> fixed;
  if(size.supplied)fixed.push_back("size");
  if(prob.supplied)fixed.push_back("prob");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",res.size},{"prob",res.prob}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
