#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <limits>
namespace umvl_binding {
inline FitResult fit_bnbinom(cppunivariate::ispan x, Held size_arg = {}, Held alpha_arg = {}, Held beta_arg = {}, double gradtol=1e-8, int iterlim=600) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held size = size_arg;
  const Held alpha = alpha_arg;
  const Held beta = beta_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  for (const auto &parameter : {size,alpha,beta}) {
    if (parameter.supplied) {
      const double value=parameter.value;
      if (!(value>0) || !std::isfinite(value))
        return fail("Fixed parameters must be positive finite numbers.");
    }
  }
  const auto fit=cppunivariate::bnbinom_mle(x,gradtol,iterlim,
    size.supplied?size.value:nan,
    alpha.supplied?alpha.value:nan,
    beta.supplied?beta.value:nan);
  std::string stop_kind;
  switch(fit.status) {
    case cppunivariate::BnbinomStatus::NegativeBinomialLimit: stop_kind="limit";break;
    case cppunivariate::BnbinomStatus::PoissonLimit: stop_kind="limit";break;
    case cppunivariate::BnbinomStatus::PointMass: stop_kind="limit";break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,fit.stop_detail,stop_kind,{"size","alpha","beta"});
  };

  switch (fit.status) {
    case cppunivariate::BnbinomStatus::Ok: break;
    case cppunivariate::BnbinomStatus::NegativeBinomialLimit:
      return stopped_fail("The negative binomial face wins (including likelihood ties). Use `mlnbinom`; hold its size at the supplied `size` or `beta`, if either was held.");
    case cppunivariate::BnbinomStatus::PoissonLimit:
      return stopped_fail("The Poisson face wins (including likelihood ties). Use `mlpois` instead.");
    case cppunivariate::BnbinomStatus::PointMass:
      return stopped_fail("The maximum likelihood estimator does not exist: all observations are zero (point mass). Use `mlpois` instead.");
    case cppunivariate::BnbinomStatus::InvalidInput:
      return stopped_fail("Beta negative binomial fitting requires nonempty nonnegative counts, positive finite fixed parameters and gradtol, and positive iterlim.");
    default: return stopped_fail("Beta negative binomial search failed: unresolved stationary point or search range; try increasing iterlim.");
  }
  std::vector<std::string> fixed;
  if (size.supplied) fixed.push_back("size");
  if (alpha.supplied) fixed.push_back("alpha");
  if (beta.supplied) fixed.push_back("beta");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",fit.size},{"alpha",fit.alpha},{"beta",fit.beta}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
