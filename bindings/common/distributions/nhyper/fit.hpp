#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/nhyper/mle.hpp"
#include <limits>
#include <cmath>
namespace umvl_binding {
inline FitResult fit_nhyper(cppunivariate::span x, Held m_arg = {}, Held N_arg = {}, Held r_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held m = m_arg;
  const Held N = N_arg;
  const Held r = r_arg;

  const double design=!r.supplied ? std::numeric_limits<double>::quiet_NaN() : r.value;
  if (r.supplied && std::isnan(design)) return fail("`r` must be a whole number or NULL, not NA.");
  const double held = !m.supplied ? std::numeric_limits<double>::quiet_NaN() : m.value;
  if (m.supplied && std::isnan(held)) return fail("`m` must be a whole number or NULL, not NA.");
  const double population = !N.supplied ? std::numeric_limits<double>::quiet_NaN() : N.value;
  if (N.supplied && std::isnan(population)) return fail("`N` must be a whole number or NULL, not NA.");
  const auto fit = cppunivariate::nhyper_mle(x, design, population, held);
  switch (fit.status) {
    case cppunivariate::NhyperStatus::FaceUnresolved:
      return fail("No finite maximum was found in the exploratory search; a larger finite maximum is not excluded. For the negative-binomial growing-urn limit use mlnbinom(x - r, size = r).");
    case cppunivariate::NhyperStatus::FaceTie:
      return fail("A finite likelihood matched the negative-binomial face within numerical tolerance; a larger finite maximum is not excluded. Use mlnbinom(x - r, size = r) for the limit model.");
    case cppunivariate::NhyperStatus::NumericalFailure:
      return fail("Finite-grid numerical failure: 1e7 candidate evaluation budget or representability/nonfinite likelihood limit exceeded.");
    case cppunivariate::NhyperStatus::InvalidInput:
      return fail("Counts must be whole numbers from 0 to 2^53 - 1, with r <= m <= N.");
    case cppunivariate::NhyperStatus::Infeasible:
      return fail("No admissible composition gives every observation positive probability; total draws must lie from r through N - m + r (zero when r = 0).");
    case cppunivariate::NhyperStatus::Tie:
      warnings.push_back(binding_message("Likelihood tied between adjacent counts; returning the smaller.")); break;
    case cppunivariate::NhyperStatus::Unidentified:
      warnings.push_back(binding_message("With r = 0 every composition is unidentified; returning m = 0.")); break;
    case cppunivariate::NhyperStatus::Ok: break;
  }
  std::vector<std::string> fixed;
  if (m.supplied) fixed.push_back("m");
  if (N.supplied) fixed.push_back("N");
  if (r.supplied) fixed.push_back("r");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"m",fit.m},{"N",fit.N},{"r",fit.r}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
