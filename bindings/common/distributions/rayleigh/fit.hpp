#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/rayleigh/mle.hpp"
#include <limits>
#include <cmath>
namespace umvl_binding {
inline FitResult fit_rayleigh(cppunivariate::span x, Held sigma_arg = {}) {
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v) && v>=0;}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
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
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (res.status == cppunivariate::RayleighStatus::Ok)
        return overridden(std::move(out), static_cast<int>(res.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(res.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
  if (res.status == cppunivariate::RayleighStatus::ZeroLikelihood) {
    return done(classify(fail("The Rayleigh likelihood is identically zero when any observation is zero; "
               "no scale has positive likelihood."),{},outcome_kind(o),{},""));
  }
  if (res.status != cppunivariate::RayleighStatus::Ok || !std::isfinite(res.logLik)) return done(fail("Invalid Rayleigh data."));
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"sigma",res.sigma}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
