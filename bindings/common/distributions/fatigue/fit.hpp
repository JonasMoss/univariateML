#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/fatigue/mle.hpp"
#include <cmath>
#include <algorithm>
#include <limits>
namespace umvl_binding {
inline FitResult fit_fatigue(cppunivariate::span x, Held alpha_arg = {}, Held beta_arg = {}, Held mu_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(abstol>0 && std::isfinite(abstol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, "nomle", {}); };
  const Held alpha = alpha_arg;
  const Held beta = beta_arg;
  const Held mu = mu_arg;

  if (!mu.supplied) return fail("The threshold mu cannot be estimated. With mu free, the likelihood is unbounded for every sample with a unique minimum, so no maximum likelihood estimate exists. Supply mu.");
  const double threshold = mu.value;
  if (!std::isfinite(threshold)) return fail("`mu` must be finite.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=alpha.supplied?alpha.value:nan;
  const double b=beta.supplied?beta.value:nan;
  if ((alpha.supplied&&(!(a>0)||!std::isfinite(a))) ||
      (beta.supplied&&(!(b>0)||!std::isfinite(b))))
    return fail("Held `alpha` and `beta` must be positive finite numbers.");
  auto res = cppunivariate::fatigue_mle_shifted(x, threshold, abstol, iterlim,a,b);
  if (res.status == cppunivariate::FatigueStatus::InvalidInput)
    return fail("Fatigue observations must be finite and strictly above `mu`.");
  if (res.status == cppunivariate::FatigueStatus::SearchFailure) {
    for(auto& candidate:res.stop_detail.candidates)candidate.coef.push_back(threshold);
    if(res.stop_detail.budget_exhausted)return classify(fail("Fatigue optimizer SearchFailure: certificate budget exhausted; increase iterlim."),res.stop_detail,"budget",{"alpha","beta","mu"});
    if(res.stop_detail.comparison_tie)return classify(fail("Fatigue optimizer SearchFailure: competing global maxima cannot be distinguished."),res.stop_detail,"tie",{"alpha","beta","mu"});
    return classify(fail("Fatigue optimizer SearchFailure: profile search did not converge or floating-point coefficient resolution failed; increase iterlim for unresolved searches."),res.stop_detail,"numerical",{"alpha","beta","mu"});
  }
  if (!(res.alpha>0.0) || !std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    if (res.status==cppunivariate::FatigueStatus::Unbounded)
      return nomle("The fatigue maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    return fail("Fatigue MLE failed: data exceed double precision or profile search did not converge; increase iterlim.");
  }
  std::vector<std::string> fixed;
  if(alpha.supplied) fixed.push_back("alpha");
  if(beta.supplied) fixed.push_back("beta");
  fixed.push_back("mu");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"alpha",res.alpha},{"beta",res.beta},{"mu",threshold}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
