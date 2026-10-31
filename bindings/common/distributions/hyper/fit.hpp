#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/hyper/mle.hpp"
#include <cmath>
#include <iomanip>
#include <limits>
#include <sstream>
#include <string>
namespace umvl_binding {
inline FitResult fit_hyper(cppunivariate::span x, Held m_arg = {}, Held N_arg = {}, Held k_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held m = m_arg;
  const Held N = N_arg;
  const Held k = k_arg;

  if (!N.supplied && !m.supplied && !k.supplied)
    return fail("Joint estimation of m, N and k is not implemented: the interior and binomial and Poisson limit faces require a certified global comparison.");
  if (!N.supplied && !m.supplied) return fail("Joint estimation of N and m is not implemented; it can escape to the binomial limit, for which use mlbinom(x, size = k).");
  if (!k.supplied && !N.supplied) return fail("Joint estimation with k free is not supported by this fitter.");
  for (const auto& named : {std::make_pair(std::string("k"), k),
                            std::make_pair(std::string("N"), N),
                            std::make_pair(std::string("m"), m)}) {
    if (named.second.supplied && std::isnan(named.second.value))
      return fail("`%s` must be a whole number or NULL, not NA.", named.first);
  }
  const auto held_value = [](const Held& value, const std::string&) {
    return value.supplied ? value.value : std::numeric_limits<double>::quiet_NaN();
  };
  const auto whole = [](double v) {
    std::ostringstream out;
    out << std::fixed << std::setprecision(0) << v;
    return out.str();
  };
  const double design=held_value(k, "k");
  const double N_held = held_value(N, "N"), m_held = held_value(m, "m");
  const auto res = cppunivariate::hyper_mle(x, design, N_held, m_held);
  const std::string estimated = !k.supplied ? "k" : (!N.supplied ? "N" : "m");
  switch (res.status) {
    case cppunivariate::HyperStatus::NumericalFailure:
      return fail("Finite-grid numerical failure: 1e7 candidate evaluation budget or representability/nonfinite likelihood limit exceeded.");
    case cppunivariate::HyperStatus::BothFree:
      return fail("Supply the population size `N` or the number of successes `m` "
                 "as well as `k`: estimating both is not supported. As `N` grows "
                 "with `m / N` fixed the model tends to the binomial; use "
                 "`mlbinom(x, size = k)`.");
    case cppunivariate::HyperStatus::InvalidInput:
      return fail("`k`, `N` and `m` must be whole numbers from 0 to 2^53 - 1, with "
                 "`k <= N` and `m <= N`; so must the observations.");
    case cppunivariate::HyperStatus::Infeasible:
      return fail("No admissible value of `%s` gives every observation positive "
                 "probability: observations must lie between max(0, k - (N - m)) "
                 "and min(k, m).", estimated);
    case cppunivariate::HyperStatus::AllZero:
      return fail("All observations are 0: with `m` and `k` positive the likelihood "
                 "increases toward a point mass at zero as `N` grows, so the "
                 "maximum likelihood estimator of `N` does not exist.");
    case cppunivariate::HyperStatus::TooLarge:
      return fail("The required population size or likelihood maximiser exceeds "
                 "2^53 - 1; double precision does not represent every integer "
                 "beyond that, so the "
                 "estimate of `N` cannot be computed exactly.");
    case cppunivariate::HyperStatus::Tie:
      warnings.push_back(binding_message("The likelihood is tied (to rounding) between `%s` = %s and %s; "
                    "returning the smaller.", estimated,
                    whole(!k.supplied ? res.k : (!N.supplied ? res.N : res.m)), whole(res.tie)));
      break;
    case cppunivariate::HyperStatus::Unidentified:
      if (std::isinf(res.tie)) {
        warnings.push_back(binding_message("Every `N` from %s upwards attains the maximum likelihood "
                      "(`k = 0` or `m = 0`); returning `N` = %s.",
                      whole(res.N), whole(res.N)));
      } else if (!k.supplied) {
        warnings.push_back(binding_message("Every `k` from %s to %s attains the maximum likelihood; returning the smaller.", whole(res.k), whole(res.tie)));
      } else {
        warnings.push_back(binding_message("With `k = 0` every `m` from %s to %s attains the maximum "
                      "likelihood; returning `m` = %s.", whole(res.m),
                      whole(res.tie), whole(res.m)));
      }
      break;
    case cppunivariate::HyperStatus::Ok:
      break;
  }
  std::vector<std::string> fixed;
  if (m.supplied) fixed.push_back("m");
  if (N.supplied) fixed.push_back("N");
  if (k.supplied) fixed.push_back("k");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"m",res.m},{"N",res.N},{"k",res.k}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
