#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/power/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_power(cppunivariate::span x, Held alpha_arg = {}, Held beta_arg = {}, double epsilon = 1.490116e-08) {
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
  const Held alpha = alpha_arg;
  const Held beta = beta_arg;

  std::vector<std::string> fixed;
  const double alpha_held = !alpha.supplied ? std::numeric_limits<double>::quiet_NaN() : alpha.value;
  if (alpha.supplied) {
    if (!(std::isfinite(alpha_held) && alpha_held > 0.0)) return invalid_argument(fail("Held `alpha` must be positive and finite."));
    fixed.push_back("alpha");
  }
  const double beta_held = !beta.supplied ? std::numeric_limits<double>::quiet_NaN() : beta.value;
  if (beta.supplied) {
    if (!(std::isfinite(beta_held) && beta_held > 0.0)) return invalid_argument(fail("Held `beta` must be positive and finite."));
    fixed.push_back("beta");
  }
  if (alpha.supplied && std::any_of(x.begin(),x.end(),[&](double v){return v>alpha_held;})) return invalid_argument(fail("Held alpha must be at least the sample maximum: zero likelihood."));

  if (!(epsilon>=0.0) || !std::isfinite(epsilon))
    return invalid_argument(fail("`epsilon` must be nonnegative and finite."));
  auto res = cppunivariate::power_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, epsilon, alpha_held, beta_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, outcome_kind(o), {}); };

  if (res.status == cppunivariate::PowerStatus::Unbounded)
    return done(nomle("Power likelihood is unbounded as beta -> Inf when every observation equals alpha; hold beta to fit constant samples."));
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    return done(fail("The power distribution is only defined for positive numbers."));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"alpha",res.alpha},{"beta",res.beta}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
