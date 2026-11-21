#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/sgomp/mle.hpp"
namespace umvl_binding {
inline FitResult fit_sgomp(cppunivariate::span x, Held b_arg = {}, Held eta_arg = {}, double tol=1e-10, int iterlim=200) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held b = b_arg;
  const Held eta = eta_arg;

  const double bv=b.supplied ? b.value : cppunivariate::sgomp_detail::nan();
  const double ev=eta.supplied ? eta.value : cppunivariate::sgomp_detail::nan();
  if (b.supplied && !(bv>0 && std::isfinite(bv))) return invalid_argument(fail("fixed `b` must be positive and finite."));
  if (eta.supplied && !(ev>=0 && std::isfinite(ev))) return invalid_argument(fail("fixed `eta` must be nonnegative and finite."));
  const auto f=cppunivariate::sgomp_mle(x,tol,iterlim,bv,ev);
  const auto o = cppunivariate::outcome(f);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(f.status));
    return with_outcome(std::move(out), o, static_cast<int>(f.status),{{"b",b_arg.supplied},{"eta",eta_arg.supplied}});
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };

  if(f.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("sgomp numerical accuracy failure: requested coefficient accuracy cannot be established."),f.stop_detail,outcome_payload(o),{"b","eta"}));
  if (f.status == cppunivariate::SgompStatus::ShapeOverflow)
    return done(fail("Shifted Gompertz shape at the maximum exceeds double precision range."));
  if (f.status==cppunivariate::SgompStatus::Collapse) return done(nomle("The shifted Gompertz MLE does not exist: constant data have unbounded likelihood."));
  if (f.status==cppunivariate::SgompStatus::InvalidInput) return done(fail("Shifted Gompertz requires finite nonnegative data, positive tol and positive iterlim."));
  if (f.status!=cppunivariate::SgompStatus::Ok) {
    if(f.stop_detail.budget_exhausted)return done(stop_failure(fail("Shifted Gompertz certificate budget exhausted; increase iterlim."),f.stop_detail,outcome_payload(o),{"b","eta"}));
    if(f.stop_detail.comparison_tie)return done(comparison_payload(stop_failure(fail("Shifted Gompertz competing global maxima cannot be distinguished; increase iterlim."),f.stop_detail,PayloadKind::Comparison,{"b","eta"}),o,cppunivariate::comparison_choice(f),{{"b",b_arg},{"eta",eta_arg}}));
    return done(stop_failure(fail("Shifted Gompertz MLE failed to converge; increase iterlim."),f.stop_detail,outcome_payload(o),{"b","eta"}));
  }
  std::vector<std::string> fixed;
  if (b.supplied) fixed.push_back("b");
  if (eta.supplied) fixed.push_back("eta");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"b",f.b},{"eta",f.eta}}; out.logLik = f.logLik; out.iterations = f.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
