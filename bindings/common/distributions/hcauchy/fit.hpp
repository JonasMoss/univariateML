#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/hcauchy/mle.hpp"
namespace umvl_binding {
inline FitResult fit_hcauchy(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}) {
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, "nomle", {}); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

  const bool mu_held = mu.supplied;
  const double held_mu = mu_held ? mu.value : std::numeric_limits<double>::quiet_NaN();
  if (mu_held && !std::isfinite(held_mu)) return fail("`mu` must be finite.");
  const double sc=sigma.supplied?sigma.value:std::numeric_limits<double>::quiet_NaN();
  if (sigma.supplied && (!(sc>0)||!std::isfinite(sc))) return fail("`sigma` must be positive finite.");
  const auto r=cppunivariate::hcauchy_mle(x,held_mu,sc);
  if(r.status==cppunivariate::HcauchyStatus::Unbounded)
    return nomle("Half-Cauchy likelihood is unbounded as sigma -> 0 when more than half the observations equal the free or held endpoint (including all equal to it).");
  if(r.status==cppunivariate::HcauchyStatus::ScaleBoundary)
    return classify(fail("Half-Cauchy likelihood has a finite unattained supremum as sigma -> 0 when exactly half the observations equal the free or held endpoint."),{},"nomle",{},"");
  if(mu_held && r.status==cppunivariate::HcauchyStatus::InvalidData)
    return fail("Half-Cauchy observations must be finite and at least the held mu.");
  if(r.status!=cppunivariate::HcauchyStatus::Ok)
    return fail("Half-Cauchy MLE requires finite data and a representable positive scale.");
  std::vector<std::string> fixed;
  if(mu_held) fixed.push_back("mu");
  if(sigma.supplied) fixed.push_back("sigma");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"sigma",r.sigma}}; out.logLik = r.logLik; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
