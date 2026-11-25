#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/triang/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_triang(cppunivariate::span x, Held a_arg = {}, Held b_arg = {}, Held c_arg = {}, double tol = 1e-10, int iterlim = 100) {

  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...)); };
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

  auto r=cppunivariate::triang_mle(x,tol,iterlim,checked_value_0,checked_value_1,checked_value_2);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(r.status),{{"a",a_arg.supplied},{"b",b_arg.supplied},{"c",c_arg.supplied}});
  };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...)), {}, outcome_payload(o), {}); };
  if(r.status==cppunivariate::TriangStatus::Unbounded)
    return done(nomle("Triangular likelihood is unbounded for constant data with these fixed parameters."));
  if(r.status==cppunivariate::TriangStatus::Incompatible)
    return done(fail("Data incompatible with held triangular parameters (outside a held endpoint or zero likelihood)."));
  if(r.status==cppunivariate::TriangStatus::InvalidInput)
    return done(fail("Invalid triangular inputs: require finite data, a < b, a <= c <= b, positive tol and iterlim."));
  if (r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)
    return done(stop_failure(fail("Triangular coefficient accuracy cannot be established at the requested `tol`."),
        r.stop_detail,outcome_payload(o),{"a","b","c"}));
  if(r.status==cppunivariate::TriangStatus::NumericalFailure)
    return done(stop_failure(fail("Triangular public coefficients lose likelihood beyond the numerical certificate margin."),{},outcome_payload(o),{"a","b","c"}));
  if(r.status==cppunivariate::TriangStatus::IterationFailure)
    return done(fail("Triangular endpoint solve did not converge; increase iterlim."));
  std::vector<std::string> fixed;
  if(a.supplied) fixed.push_back("a");
  if(b.supplied) fixed.push_back("b");
  if(c.supplied) fixed.push_back("c");
  FitResult out;
  out.estimates = {{"a",r.a},{"b",r.b},{"c",r.c}};
  out.logLik = r.logLik;
  out.iterations = r.iterations;
  out.has_iterations = true;
  out.fixed = fixed;
  out.has_fixed = true;
  attach_maximizer_set(out, r.maximizers, "triang", [&](const auto& c) {
    double ll = 0;
    for (double v : x) ll += cppunivariate::distributions::dtriang(v,c[0],c[1],c[2],true);
    return ll;
  });
  if (r.not_unique && !o.uniqueness_known) out.candidates.push_back({out.estimates,r.logLik,r.logLik,"triang"});
  if (r.reflection_tie && !r.maximizers.size) {
    const double minimum = *std::min_element(x.begin(), x.end());
    const double maximum = *std::max_element(x.begin(), x.end());
    const auto reflect = [&](double value) {
      return std::abs(value-minimum) <= std::abs(value-maximum)
          ? maximum-(value-minimum) : minimum-(value-maximum);
    };
    out.candidates.push_back({{{"a",a.supplied ? r.a : reflect(r.b)},
                               {"b",b.supplied ? r.b : reflect(r.a)},
                               {"c",reflect(r.c)}},r.logLik,r.logLik,"triang"});
    out.error = "The reflected triangular fit has the same likelihood by symmetry, with completeness or coefficient accuracy unresolved.";
  }
  for (const auto& rival : r.unresolved_modes)
    out.candidates.push_back({{{"a",rival.a},{"b",rival.b},{"c",rival.c}},
                               rival.logLik,rival.logLik,"triang"});
  if (!r.unresolved_modes.empty()) {
    if (!out.error.empty()) out.error += " ";
    out.error += "The likelihood comparison between candidate triangular modes could not be resolved.";
  }
  return done(std::move(out));
}
}
