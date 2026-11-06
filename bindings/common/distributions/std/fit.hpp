#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/std/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_std(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}, Held nu_arg = {}, double gradtol = 1.220703e-4, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held sd = sd_arg;
  const Held nu = nu_arg;

  if(nu.supplied&&!std::isfinite(nu.value))return fail("`nu` must be finite and greater than 2.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.supplied?mean.value:nan;
  if(mean.supplied && (!std::isfinite(held_mean)))return fail("Invalid held `mean`.");
  const double held_sd=sd.supplied?sd.value:nan;
  if(sd.supplied && (!std::isfinite(held_sd) || !(held_sd>0)))return fail("Invalid held `sd`.");
  std::vector<std::string> fixed;
  if(mean.supplied)fixed.push_back("mean");
  if(sd.supplied)fixed.push_back("sd");
  if(nu.supplied)fixed.push_back("nu");
  auto res=cppunivariate::student_t_mle(x,gradtol,iterlim,held_mean,held_sd,nu.supplied?nu.value:nan);
  if(res.status==cppunivariate::StudentTStatus::CertificateBudget)return stop_failure(fail("Student-t global certificate budget exhausted; no verified MLE."),res.stop_detail,"budget",{"mean","sd","nu"});
  if(res.status==cppunivariate::StudentTStatus::CertificateTie)return stop_failure(fail("Student-t global certificate unresolved tie or sign-uncertain seam; no verified MLE."),res.stop_detail,"tie",{"mean","sd","nu"});
  if(res.status==cppunivariate::StudentTStatus::NumericalFailure)return stop_failure(fail("Student-t global certificate numerical failure: score/root, enclosure, domain or linear solve unresolved; no verified MLE."),res.stop_detail,"numerical",{"mean","sd","nu"});
  if (res.status == cppunivariate::StudentTStatus::InvalidNu)
    return fail("`nu` must be finite and greater than 2.");
  if (res.status == cppunivariate::StudentTStatus::Unbounded)
    return stop_failure(fail("Likelihood is unbounded at a tie of excessive multiplicity k > nu*(n-k) (free df: k > 2*(n-k)); no MLE."),res.stop_detail,"nomle",{"mean","sd","nu"});
  if (res.status == cppunivariate::StudentTStatus::ScaleBoundary)
    return stop_failure(fail("Tie equality scale-collapse boundary; a regular MLE is not certified."),res.stop_detail,"nomle",{"mean","sd","nu"},(!mean.supplied&&!sd.supplied&&!nu.supplied)?"t scale collapse (no package family)":"");
  if (res.status == cppunivariate::StudentTStatus::IterationFailure)
    return fail("Student-t optimizer IterationFailure: did not converge to the score tolerance; increase iterlim.");
  if (res.status == cppunivariate::StudentTStatus::NormalBoundary) {
    return stop_failure(fail("The Student-t likelihood is maximised as nu -> Inf, i.e. by the "
               "normal distribution. Use `mlnorm` to fit this model."),res.stop_detail,"limit",{"mean","sd","nu"},"norm");
  }
  if (res.status == cppunivariate::StudentTStatus::DfTwoBoundary) {
    return stop_failure(fail("The Student-t likelihood is maximised as nu -> 2 (infinite "
               "variance); the MLE does not exist in the mean/sd parameterisation. "
               "Use `mllst` to fit a location-scale t distribution with free degrees of freedom."),res.stop_detail,"limit",{"mean","sd","nu"},"lst");
  }
  if (res.status == cppunivariate::StudentTStatus::BfgsFailure)
    return fail("StudentT optimizer BfgsFailure: score check failed; increase iterlim.");
  if (res.status == cppunivariate::StudentTStatus::InvalidStart) {
    return fail("Student-t MLE requires at least 2 finite observations with "
               "positive variance.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu)) {
    return fail("Student-t MLE failed to converge (check inputs)");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",res.mu},{"sd",res.sigma},{"nu",res.nu}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
