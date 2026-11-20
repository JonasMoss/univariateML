#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/std/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_std(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}, Held nu_arg = {}, double tol = 1e-10, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) {
    auto out=failure(binding_message(format, arguments...), warnings);

    return out;
  };
  const Held mean = mean_arg;
  const Held sd = sd_arg;
  const Held nu = nu_arg;

  if(nu.supplied&&!std::isfinite(nu.value))return invalid_argument(fail("`nu` must be finite and greater than 2."));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.supplied?mean.value:nan;
  if(mean.supplied && (!std::isfinite(held_mean)))return invalid_argument(fail("Invalid held `mean`."));
  const double held_sd=sd.supplied?sd.value:nan;
  if(sd.supplied && (!std::isfinite(held_sd) || !(held_sd>0)))return invalid_argument(fail("Invalid held `sd`."));
  std::vector<std::string> fixed;
  if(mean.supplied)fixed.push_back("mean");
  if(sd.supplied)fixed.push_back("sd");
  if(nu.supplied)fixed.push_back("nu");
  auto res=cppunivariate::student_t_mle(x,tol,iterlim,held_mean,held_sd,nu.supplied?nu.value:nan);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"mean",mean_arg.supplied},{"sd",sd_arg.supplied},{"nu",nu_arg.supplied}});
  };
  if(res.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("std numerical accuracy failure: requested coefficient accuracy cannot be established."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if(res.accuracy_failure)return done(stop_failure(fail("Student-t numerical accuracy failure: observed-information Newton refinement could not justify the requested coefficient tolerance; increase iterlim or inspect conditioning."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if(res.status==cppunivariate::StudentTStatus::CertificateBudget)return done(stop_failure(fail("Student-t global certificate budget exhausted; no verified MLE. Raising iterlim may resolve this stop."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if(res.status==cppunivariate::StudentTStatus::NonUniqueHeldSd)return done(comparison_payload(stop_failure(fail("With sd held, this balanced two-point sample has distinct global Student-t maximizers by symmetry and the variance bound. Candidates are finite likelihood witnesses."),res.stop_detail,PayloadKind::Comparison,{"mean","sd","nu"}),o,cppunivariate::comparison_choice(res),{{"mean",mean_arg},{"sd",sd_arg},{"nu",nu_arg}}));
  if(res.status==cppunivariate::StudentTStatus::CertificateTie)return done(comparison_payload(stop_failure(fail("Student-t global certificate unresolved tie or sign-uncertain seam; no verified MLE."),res.stop_detail,PayloadKind::Comparison,{"mean","sd","nu"}),o,cppunivariate::comparison_choice(res),{{"mean",mean_arg},{"sd",sd_arg},{"nu",nu_arg}}));
  if(res.status==cppunivariate::StudentTStatus::NumericalFailure && !nu.supplied && (mean.supplied || sd.supplied))
    return done(stop_failure(fail(sd.supplied ? "Student-t held-subset global certificate unresolved: normal (`mlnorm`) or scale-collapse comparisons, finite roots or arithmetic bounds remain unresolved; no verified MLE." : "Student-t held-subset global certificate unresolved: normal (`mlnorm`) or positive-scale t2 (`mllst`) comparisons, finite roots or arithmetic bounds remain unresolved; no verified MLE."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if(res.status==cppunivariate::StudentTStatus::NumericalFailure)return done(stop_failure(fail("Student-t global certificate numerical failure: score/root, enclosure, domain or linear solve unresolved; no verified MLE."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if (res.status == cppunivariate::StudentTStatus::InvalidNu)
    return done(fail("`nu` must be finite and greater than 2."));
  if (res.status == cppunivariate::StudentTStatus::Unbounded)
    return done(stop_failure(fail("Likelihood is unbounded at a tie of excessive multiplicity k > nu*(n-k) (free df: k > 2*(n-k)); no MLE."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if (res.status == cppunivariate::StudentTStatus::ScaleBoundary)
    return done(stop_failure(fail("Tie equality scale-collapse boundary; a regular MLE is not certified."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"},(!mean.supplied&&!sd.supplied&&!nu.supplied)?"unavailable:t-scale-collapse":""));
  if (res.status == cppunivariate::StudentTStatus::IterationFailure)
    return done(stop_failure(fail("Student-t optimizer IterationFailure: did not converge to the score tolerance; increase iterlim."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if (res.status == cppunivariate::StudentTStatus::NormalBoundary) {
    return done(stop_failure(fail("The Student-t likelihood is maximised as nu -> Inf, i.e. by the "
               "normal distribution. Use `mlnorm` to fit this model."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"},"norm"));
  }
  if (res.status == cppunivariate::StudentTStatus::DfTwoBoundary) {
    return done(stop_failure(fail("The Student-t likelihood is maximised as nu -> 2 (infinite "
               "variance); the MLE does not exist in the mean/sd parameterisation. "
               "Use `mllst` to fit a location-scale t distribution with free degrees of freedom."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"},"lst"));
  }
  if (res.status == cppunivariate::StudentTStatus::BfgsFailure)
    return done(stop_failure(fail("StudentT optimizer BfgsFailure: score check failed; increase iterlim."),res.stop_detail,outcome_payload(o),{"mean","sd","nu"}));
  if (res.status == cppunivariate::StudentTStatus::InvalidStart) {
    return done(fail("Student-t MLE requires at least 2 finite observations with "
               "positive variance."));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",res.mu},{"sd",res.sigma},{"nu",res.nu}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true;  return out; }());

}
}
