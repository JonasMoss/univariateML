#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/logitnorm/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_logitnorm(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}) {
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, "nomle", {}); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const double nan = std::numeric_limits<double>::quiet_NaN();

  const double checked_mu = mu.supplied ? mu.value : nan;
  if (mu.supplied && (!std::isfinite(checked_mu) || (false && !(checked_mu > 0)))) return fail("Held parameters must be finite and shapes/scales positive.");
  const double checked_sigma = sigma.supplied ? sigma.value : nan;
  if (sigma.supplied && (!std::isfinite(checked_sigma) || (true && !(checked_sigma > 0)))) return fail("Held parameters must be finite and shapes/scales positive.");

  std::vector<std::string> fixed;
  if (mu.supplied) fixed.push_back("mu");
  if (sigma.supplied) fixed.push_back("sigma");
  auto res = cppunivariate::logitnorm_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, checked_mu, checked_sigma);
  if (res.status == cppunivariate::LogitnormStatus::Unbounded) {
    return nomle("The logitnorm maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as scale shrinks to zero.");
  }
  if (res.status == cppunivariate::LogitnormStatus::InvalidInput) return fail("Logit-normal requires nonempty finite data strictly between zero and one and valid held parameters.");
  if (res.status != cppunivariate::LogitnormStatus::Ok || !std::isfinite(res.logLik))
    return fail("Logit-normal MLE failed: likelihood cannot be resolved in double precision.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",res.mu},{"sigma",res.sigma}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; return out; }();

}
}
