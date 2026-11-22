#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/nhyper/mle.hpp"
#include <limits>
#include <cmath>
namespace umvl_binding {
inline FitResult fit_nhyper(cppunivariate::span x, Held m_arg = {}, Held N_arg = {}, Held r_arg = {}, int iterlim = 100) {
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...)); };
  const Held m = m_arg;
  const Held N = N_arg;
  const Held r = r_arg;

  const double design=!r.supplied ? std::numeric_limits<double>::quiet_NaN() : r.value;
  if (r.supplied && std::isnan(design)) return invalid_argument(fail("`r` must be a whole number or NULL, not NA."));
  const double held = !m.supplied ? std::numeric_limits<double>::quiet_NaN() : m.value;
  if (m.supplied && std::isnan(held)) return invalid_argument(fail("`m` must be a whole number or NULL, not NA."));
  const double population = !N.supplied ? std::numeric_limits<double>::quiet_NaN() : N.value;
  if (N.supplied && std::isnan(population)) return invalid_argument(fail("`N` must be a whole number or NULL, not NA."));
  if (iterlim < 1) return invalid_argument(fail("`iterlim` must be at least 1."));
  const auto fit = cppunivariate::nhyper_mle(x, design, population, held, iterlim);
  const auto o = cppunivariate::outcome(fit);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(fit.status),{{"m",m_arg.supplied},{"N",N_arg.supplied},{"r",r_arg.supplied}});
  };
  PayloadKind stop_kind=outcome_payload(o);
  switch(fit.status) {
    case cppunivariate::NhyperStatus::FaceTie: stop_kind=PayloadKind::Comparison;break;
    case cppunivariate::NhyperStatus::FaceUnresolved: stop_kind=outcome_payload(o);break;
    case cppunivariate::NhyperStatus::Infeasible: stop_kind=outcome_payload(o);break;
    default:break;
  }
  if(fit.stop_detail.budget_exhausted) stop_kind=outcome_payload(o);
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    if(stop_kind==PayloadKind::None)return out;
    return comparison_payload(stop_failure(out,fit.stop_detail,stop_kind,{"m","N","r"}),o,
      cppunivariate::comparison_choice(fit),{{"m",m_arg},{"N",N_arg},{"r",r_arg}});
  };

  switch (fit.status) {
    case cppunivariate::NhyperStatus::FaceUnresolved:
      return done(stopped_fail("No finite maximum was found in the exploratory search; a larger finite maximum is not excluded. For the negative-binomial growing-urn limit use mlnbinom(x - r, size = r)."));
    case cppunivariate::NhyperStatus::FaceTie:
      return done(stopped_fail("A finite likelihood matched the negative-binomial face within numerical tolerance; a larger finite maximum is not excluded. Use mlnbinom(x - r, size = r) for the limit model."));
    case cppunivariate::NhyperStatus::NumericalFailure:
      return done(stopped_fail("The integer likelihood search did not converge within its work or representability limits."));
    case cppunivariate::NhyperStatus::InvalidInput:
      return done(stopped_fail("Counts must be whole numbers from 0 to 2^53 - 1, with r <= m <= N."));
    case cppunivariate::NhyperStatus::Infeasible:
      return done(stopped_fail("No admissible composition gives every observation positive probability; total draws must lie from r through N - m + r (zero when r = 0)."));
    case cppunivariate::NhyperStatus::Tie:
    case cppunivariate::NhyperStatus::Unidentified:
    case cppunivariate::NhyperStatus::Ok:
      break;
  }
  std::vector<std::string> fixed;
  if (m.supplied) fixed.push_back("m");
  if (N.supplied) fixed.push_back("N");
  if (r.supplied) fixed.push_back("r");
  FitResult out;
  out.estimates = {{"m",fit.m},{"N",fit.N},{"r",fit.r}};
  out.logLik = fit.logLik;
  out.iterations = fit.iterations;
  out.has_iterations = true;
  out.fixed = fixed;
  out.has_fixed = true;
  if (!o.unique) {
    const auto add = [&](double m_value, double N_value, double r_value) {
      out.candidates.push_back({{{"m",m_value},{"N",N_value},{"r",r_value}},
                                out.logLik,out.logLik,"nhyper"});
    };
    add(fit.m, fit.N, fit.r);
    if (!N.supplied && std::isfinite(fit.tie)) {
      add(fit.m, fit.tie, fit.r);
      out.error = "Exactly the two listed adjacent population sizes N maximize the likelihood.";
    } else if (fit.status == cppunivariate::NhyperStatus::Tie) {
      add(r.supplied ? fit.tie : fit.m, fit.N, r.supplied ? fit.r : fit.tie);
      out.error = "Likelihood tied between adjacent counts; returning the smaller.";
    } else if (fit.r == 0) {
      if (N.supplied) {
        add(fit.N, fit.N, 0);
        out.error = "With r = 0 every composition from 0 to N is unidentified; returning m = 0.";
      } else if (m.supplied) {
        add(fit.m, fit.N + 1, 0);
        out.error = "With r = 0 every integer population size m <= N <= 2^53 - 1 maximizes the likelihood.";
      } else {
        add(0, 1, 0);
        out.error = "With r = 0 every integer pair 0 <= m <= N <= 2^53 - 1 maximizes the likelihood.";
      }
    } else {
      if (cppunivariate::hyper_detail::is_count(fit.N+1))
        add(fit.N+1, fit.N+1, fit.r);
      out.error = "The observations are constant, so with m = N every admissible population size N >= r gives the same (point-mass) likelihood; the maximum is not unique.";
    }
  }
  return done(std::move(out));
}
}
