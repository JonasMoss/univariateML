#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/snorm/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_snorm(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}, Held xi_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held sd = sd_arg;
  const Held xi = xi_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.supplied ? mean.value : nan;
  if(mean.supplied && (!std::isfinite(held_mean))) return fail("Invalid held `mean`: requires a finite value.");
  const double held_sd=sd.supplied ? sd.value : nan;
  if(sd.supplied && (!(std::isfinite(held_sd) && held_sd>0))) return fail("Invalid held `sd`: requires a finite value in its parameter domain.");
  const double held_xi=xi.supplied ? xi.value : nan;
  if(xi.supplied && (!(std::isfinite(held_xi) && held_xi>0))) return fail("Invalid held `xi`: requires a finite value in its parameter domain.");
  auto res=cppunivariate::snorm_mle(x,1.220703e-4,200,held_mean,held_sd,held_xi);
  if(res.status==cppunivariate::SnormStatus::Unbounded) return fail("Scale collapse: the conditional skew-normal likelihood is unbounded; no MLE.");
  if(res.status==cppunivariate::SnormStatus::BfgsFailure) return fail("Conditional skew-normal fit did not converge.");
  if (res.status == cppunivariate::SnormStatus::RightHalfNormalBoundary) {
    return fail("The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal.");
  }
  if (res.status == cppunivariate::SnormStatus::LeftHalfNormalBoundary) {
    return fail("The likelihood is maximised by a translated half-normal (mode at the sample maximum); the skew-normal MLE does not exist. Use `mlhnorm` on `-x` (a half-normal reflected at the sample maximum).");
  }
  if (res.status == cppunivariate::SnormStatus::InvalidStart) {
    return fail("Skew-normal MLE requires at least 2 finite observations with "
               "positive variance.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.xi)) {
    return fail("Skew-normal MLE failed to converge (check inputs)");
  }
  std::vector<std::string> fixed;
  if(mean.supplied) fixed.push_back("mean");
  if(sd.supplied) fixed.push_back("sd");
  if(xi.supplied) fixed.push_back("xi");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",res.mu},{"sd",res.sigma},{"xi",res.xi}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
