#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/rayleigh/mle.hpp"
#include <limits>
#include <cmath>
namespace umvl_binding {
inline FitResult fit_rayleigh(cppunivariate::span x, Held sigma_arg = {}) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held sigma = sigma_arg;

  std::vector<std::string> fixed;
  const double sigma_held = !sigma.supplied ? std::numeric_limits<double>::quiet_NaN() : sigma.value;
  if (sigma.supplied) {
    if (!(std::isfinite(sigma_held) && sigma_held > 0.0)) return invalid_argument(fail("Held `sigma` must be positive and finite."));
    fixed.push_back("sigma");
  }

  auto res = cppunivariate::rayleigh_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, sigma_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"sigma",sigma_arg.supplied}});
  };
  if (res.status == cppunivariate::RayleighStatus::ZeroLikelihood) {
    return done(stop_failure(fail("The Rayleigh likelihood is identically zero when any observation is zero; "
               "no scale has positive likelihood."),{},outcome_payload(o),{},""));
  }
  if(res.status==cppunivariate::RayleighStatus::NumericalFailure)return done(fail("Rayleigh log likelihood is not representable."));
  if (res.status != cppunivariate::RayleighStatus::Ok) return done(fail("Invalid Rayleigh data."));
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"sigma",res.sigma}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
