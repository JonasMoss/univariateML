#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/ht/mle.hpp"
#include <cmath>
#include <limits>
#include <sstream>
#include <string>
namespace umvl_binding {
inline FitResult fit_ht(cppunivariate::span x, Held mu_arg = {}, Held nu_arg = {}, Held sigma_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mu = mu_arg;
  const Held nu = nu_arg;
  const Held sigma = sigma_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const bool mu_held = mu.supplied;
  const double held_mu=mu_held?mu.value:nan;
  if (mu_held && !std::isfinite(held_mu)) return fail("`mu` must be finite.");
  const double df=nu.supplied?nu.value:nan;
  const double sc=sigma.supplied?sigma.value:nan;
  if ((nu.supplied && (!(df>0)||!std::isfinite(df))) ||
      (sigma.supplied && (!(sc>0)||!std::isfinite(sc))))
    return fail("Held `nu` and `sigma` must be positive finite numbers.");
  const auto r=cppunivariate::ht_mle(x,held_mu,df,sc);
  if(mu_held && r.status==cppunivariate::HtStatus::InvalidData)
    return fail("Half-t observations must be finite and at least the held mu.");
  if(r.status==cppunivariate::HtStatus::Unbounded)
    return fail("Half-t likelihood is unbounded along sigma -> 0 with nu*(n-k) < k (k ties at the free or held endpoint); hold sigma, hold nu with nu*(n-k) > k, or (with a free endpoint) hold mu.");
  if(r.status==cppunivariate::HtStatus::ScaleBoundary)
    return fail("Half-t likelihood has a finite unattained supremum as sigma -> 0: nu*(n-k) = k.");
  const auto whole_mu = [](double v) {
    std::ostringstream out;
    if (v == std::floor(v) && std::abs(v) < 1e15) out << static_cast<long long>(v);
    else out << v;
    return out.str();
  };
  const std::string mu_hint = mu_held ? whole_mu(held_mu) : std::string("NULL");
  if(r.status==cppunivariate::HtStatus::NormalLimit && sigma.supplied)
    return fail("Half-t MLE escapes as nu -> infinity to the half-normal with sigma held; "
               "use `mlhnorm(x, mu = " + mu_hint + ", sigma = " + whole_mu(sc) + ")`.");
  if(r.status==cppunivariate::HtStatus::NormalLimit)
    return fail("Half-t MLE escapes as nu -> infinity; use `mlhnorm(x, mu = " + mu_hint + ")`.");
  if(r.status!=cppunivariate::HtStatus::Ok)
    return fail("Half-t MLE failed: check finite data at least the endpoint and numerical search range.");
  std::vector<std::string> fixed;
  if(mu_held) fixed.push_back("mu");
  if(nu.supplied) fixed.push_back("nu");
  if(sigma.supplied) fixed.push_back("sigma");
  std::vector<std::pair<std::string,std::string>> restricted;
  if(r.restricted & 4u) restricted.emplace_back("nu","lower");
  if(r.restricted & 8u) restricted.emplace_back("nu","upper");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",r.mu},{"nu",r.nu},{"sigma",r.sigma}}; out.logLik = r.logLik; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; out.iterations = r.iterations; out.has_iterations = true; return out; }();

}
}
