#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/hnorm/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_hnorm(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}) {
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
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const bool fixed = mu.supplied;
  const double held = fixed ? mu.value : std::numeric_limits<double>::quiet_NaN();
  if (fixed && !std::isfinite(held)) return invalid_argument(fail("`mu` must be finite."));
  const double sc = sigma.supplied ? sigma.value : std::numeric_limits<double>::quiet_NaN();
  if (sigma.supplied && (!(sc>0) || !std::isfinite(sc))) return invalid_argument(fail("`sigma` must be positive finite."));
  const auto res = cppunivariate::hnorm_mle(x, held, sc);
  // DegenerateData also covers empty input; the result does not retain its size.
  const auto o = (x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);})) ? cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::InvalidInput) : cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (res.status == cppunivariate::HnormStatus::Ok)
        return overridden(std::move(out), static_cast<int>(res.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(res.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
  if (sigma.supplied && res.status == cppunivariate::HnormStatus::Ok) {
    std::vector<std::string> fixed_names;
    if (fixed) fixed_names.push_back("mu");
    fixed_names.push_back("sigma");
    const double endpoint = res.mu;
    return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",endpoint},{"sigma",sc}}; out.logLik = res.logLik; out.fixed = fixed_names; out.has_fixed = true; return out; }());
  }
  if (fixed && res.status == cppunivariate::HnormStatus::DegenerateData)
    return done(classify(fail("Half-normal likelihood is unbounded as sigma -> 0 when all observations equal held mu."),{},outcome_kind(o),{},""));
  if (fixed && res.status != cppunivariate::HnormStatus::Ok)
    return done(fail("Half-normal observations must be finite and at least the held mu."));
  if (res.status == cppunivariate::HnormStatus::DegenerateData)
    return done(x.empty() ? fail("Translated half-normal MLE requires at least two observations with distinct values; degenerate data have no positive-scale MLE.") : classify(fail("Translated half-normal MLE requires at least two observations with distinct values; degenerate data have no positive-scale MLE."),{},outcome_kind(o),{}));
  if (res.status != cppunivariate::HnormStatus::Ok)
    return done(fail("Translated half-normal MLE requires finite data with a representable range."));
  if (fixed) return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",res.mu},{"sigma",res.sigma}}; out.logLik = res.logLik; out.fixed = std::vector<std::string>{"mu"}; out.has_fixed = true; return out; }());
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",res.mu},{"sigma",res.sigma}}; out.logLik = res.logLik; return out; }());

}
}
