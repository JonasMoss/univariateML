#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/norm/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_norm(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held sd = sd_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.supplied?mean.value:nan;
  if(mean.supplied && (!std::isfinite(held_mean)))return fail("`mean` must be finite.");
  const double held_sd=sd.supplied?sd.value:nan;
  if(sd.supplied && (!std::isfinite(held_sd) || !(held_sd>0)))return fail("`sd` must be positive and finite.");
  std::vector<std::string> fixed;
  if(mean.supplied)fixed.push_back("mean");
  if(sd.supplied)fixed.push_back("sd");

  auto res =
      cppunivariate::norm_mle(x,held_mean,held_sd);
  if (res.status == cppunivariate::NormStatus::Unbounded) {
    return fail("The norm maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as scale shrinks to zero.");
  }
  if (res.status == cppunivariate::NormStatus::InvalidInput) return fail("Norm requires nonempty finite data and valid held parameters.");
  if (res.status != cppunivariate::NormStatus::Ok) return fail("Norm likelihood exceeds double precision.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",res.mean},{"sd",res.sd}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; return out; }();

}
}
