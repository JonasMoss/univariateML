#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/triang/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_triang(cppunivariate::span x, Held a_arg = {}, Held b_arg = {}, Held c_arg = {}, double reltol = 1e-12, int iterlim = 100) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held a = a_arg;
  const Held b = b_arg;
  const Held c = c_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
    double checked_value_0 = std::numeric_limits<double>::quiet_NaN();
  if (a.supplied) {
const Held z = a;

    const double v=z.value;
    if(!std::isfinite(v)) return invalid_argument(fail("Fixed parameters must be finite."));
    checked_value_0 = v;

  }
  double checked_value_1 = std::numeric_limits<double>::quiet_NaN();
  if (b.supplied) {
const Held z = b;

    const double v=z.value;
    if(!std::isfinite(v)) return invalid_argument(fail("Fixed parameters must be finite."));
    checked_value_1 = v;

  }
  double checked_value_2 = std::numeric_limits<double>::quiet_NaN();
  if (c.supplied) {
const Held z = c;

    const double v=z.value;
    if(!std::isfinite(v)) return invalid_argument(fail("Fixed parameters must be finite."));
    checked_value_2 = v;

  }

  auto r=cppunivariate::triang_mle(x,reltol,iterlim,checked_value_0,checked_value_1,checked_value_2);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(r.status),{{"a",a_arg.supplied},{"b",b_arg.supplied},{"c",c_arg.supplied}});
  };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };
  if(r.status==cppunivariate::TriangStatus::Unbounded)
    return done(nomle("Triangular likelihood is unbounded for constant data with these fixed parameters."));
  if(r.status==cppunivariate::TriangStatus::Incompatible)
    return done(fail("Data incompatible with held triangular parameters (outside a held endpoint or zero likelihood)."));
  if(r.status==cppunivariate::TriangStatus::InvalidInput)
    return done(fail("Invalid triangular inputs: require finite data, a < b, a <= c <= b, positive reltol and iterlim."));
  if(r.status==cppunivariate::TriangStatus::NumericalFailure)
    return done(stop_failure(fail("Triangular public coefficients lose likelihood beyond the numerical certificate margin."),{},outcome_payload(o),{"a","b","c"}));
  if(r.status==cppunivariate::TriangStatus::IterationFailure)
    return done(fail("Triangular endpoint solve did not converge; increase iterlim."));
  if(r.not_unique) warnings.push_back(binding_message("Triangular maximum likelihood estimate is not unique (candidate log likelihoods within tolerance)."));
  std::vector<std::string> fixed;
  if(a.supplied) fixed.push_back("a");
  if(b.supplied) fixed.push_back("b");
  if(c.supplied) fixed.push_back("c");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"a",r.a},{"b",r.b},{"c",r.c}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
