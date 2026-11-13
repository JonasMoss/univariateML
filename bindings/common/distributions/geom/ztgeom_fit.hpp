#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/geom/ztgeom_mle.hpp"
namespace umvl_binding {
inline FitResult fit_ztgeom(cppunivariate::ispan x, Held prob_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held prob = prob_arg;

  if(prob.supplied && !std::isfinite(prob.value))return fail("`prob` must be finite and in its parameter domain.");

  const auto res=cppunivariate::ztgeom_mle(x,prob.supplied?prob.value:std::numeric_limits<double>::quiet_NaN());
  if(res.status==cppunivariate::ZtgeomStatus::InvalidInput)return fail("requires nonempty positive integer count data, a held coefficient in its domain, and compatible observations");
  if(!std::isfinite(res.logLik))return fail("MLE cannot be resolved at floating-point precision.");
  std::vector<std::string> fixed;if(prob.supplied)fixed.push_back("prob");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"prob",res.prob}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
