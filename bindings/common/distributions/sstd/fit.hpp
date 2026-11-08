#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/sstd/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_sstd(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}, Held nu_arg = {}, Held xi_arg = {}, double gradtol = 1.220703e-4, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held sd = sd_arg;
  const Held nu = nu_arg;
  const Held xi = xi_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.supplied ? mean.value : nan;
  if(mean.supplied && (!std::isfinite(held_mean))) return fail("Invalid held `mean`: requires a finite value.");
  const double held_sd=sd.supplied ? sd.value : nan;
  if(sd.supplied && (!(std::isfinite(held_sd) && held_sd>0))) return fail("Invalid held `sd`: requires a finite value in its parameter domain.");
  const double held_nu=nu.supplied ? nu.value : nan;
  if(nu.supplied && (!(std::isfinite(held_nu) && held_nu>2))) return fail("Invalid held `nu`: requires a finite value in its parameter domain.");
  const double held_xi=xi.supplied ? xi.value : nan;
  if(xi.supplied && (!(std::isfinite(held_xi) && held_xi>0))) return fail("Invalid held `xi`: requires a finite value in its parameter domain.");
  auto res=cppunivariate::sstd_mle(x,gradtol,iterlim,held_mean,held_sd,held_nu,held_xi);
  if(res.certified && (res.stop_detail.limit_model==cppunivariate::StopModel::Ht || res.stop_detail.limit_model==cppunivariate::StopModel::ReflectedHt))
    return stop_failure(fail("The skew Student-t likelihood has its supremum on the constrained half-t face; no finite skew MLE. Use `mlht` on signed endpoint distances."),res.stop_detail,"limit",{"mean","sd","nu","xi"});
  if(res.certified && res.stop_detail.limit_model==cppunivariate::StopModel::Norm)
    return stop_failure(fail("The symmetric Student-t likelihood is maximised as nu -> Inf; use `mlnorm`."),res.stop_detail,"limit",{"mean","sd","nu","xi"},"norm");
  if(res.certified && res.stop_detail.limit_model==cppunivariate::StopModel::Lst)
    return stop_failure(fail("The symmetric Student-t likelihood is maximised as nu -> 2 (infinite variance); use `mllst`."),res.stop_detail,"limit",{"mean","sd","nu","xi"},"lst");
  if(res.certified && res.stop_detail.limit_model==cppunivariate::StopModel::TCollapsePath)
    return stop_failure(fail("Tie equality scale-collapse boundary; the symmetric Student-t supremum is unattained."),res.stop_detail,"nomle",{"mean","sd","nu","xi"});
  if(res.status==cppunivariate::SstdStatus::CertificateFailure) {
    const auto& d=res.stop_detail;
    const std::string kind=d.budget_exhausted?"budget":d.comparison_tie?"tie":"numerical";
    const std::string message=d.budget_exhausted?"Skew Student-t certificate budget exhausted; increase `iterlim`.":d.comparison_tie?"Skew Student-t maxima are tied or numerically indistinguishable.":res.certificate_reason==1?"Nonunit-skew scale-collapse equality needs an unresolved finite/face likelihood comparison.":res.certificate_reason==4?"Skew Student-t skew-moment gamma-ratio arithmetic is unresolved for held nu >= 128; no skew estimate is certified.":res.certificate_reason==3?"Skew Student-t constrained half-t endpoint contact needs a removable shrinking-side density calculation.":res.certificate_reason==2?"Skew Student-t exterior scale-score bracket failed; increase `iterlim`.":"Skew Student-t certificate could not resolve the root enclosure or arithmetic; increase `iterlim`.";
    return stop_failure(fail(message),d,kind,{"mean","sd","nu","xi"});
  }
  if(res.status==cppunivariate::SstdStatus::UnsupportedHold) return fail("Unsupported conditional fit.");
  if (res.status == cppunivariate::SstdStatus::Unbounded)
    return stop_failure(fail("Likelihood is unbounded at a tie of multiplicity k > nu*(n-k) with nu held, or k > 2*(n-k) with nu free; no MLE."),res.stop_detail,"nomle",{"mean","sd","nu","xi"});
  if (res.status == cppunivariate::SstdStatus::ScaleBoundary)
    return stop_failure(fail(res.certified?"At tie equality the scale-collapse supremum is finite and unattained; no MLE.":"Tie equality scale-collapse boundary; a regular MLE is not certified."),res.stop_detail,res.certified?"nomle":"numerical",{"mean","sd","nu","xi"});
  if (res.status == cppunivariate::SstdStatus::InvalidStart) {
    return fail("Skew Student-t MLE requires at least 2 finite observations "
               "with positive variance.");
  }
  if (res.status == cppunivariate::SstdStatus::SnormBoundary) {
    return fail("The skew Student-t likelihood is maximised as nu -> Inf, i.e. by "
               "the skew-normal distribution. Use `mlsnorm` to fit this model.");
  }
  if (res.status == cppunivariate::SstdStatus::RightHalfNormalBoundary) {
    return fail("The skew Student-t likelihood is maximised as nu -> Inf and xi -> Inf, "
               "i.e. by a translated half-normal with endpoint at the sample minimum. "
               "Use `mlhnorm` to fit this model.");
  }
  if (res.status == cppunivariate::SstdStatus::LeftHalfNormalBoundary) {
    return fail("The skew Student-t likelihood is maximised as nu -> Inf and xi -> 0, "
               "i.e. by a reflected translated half-normal with endpoint at the sample "
               "maximum. Use `mlhnorm` on `-x` to fit this model.");
  }
  if (res.status == cppunivariate::SstdStatus::RightHalfTBoundary ||
      res.status == cppunivariate::SstdStatus::LeftHalfTBoundary) {
    const bool right = res.status == cppunivariate::SstdStatus::RightHalfTBoundary;
    if(mean.supplied || sd.supplied)
      return fail("The skew Student-t likelihood is maximised as xi -> %s by a translated half-t "
                 "with %.4g degrees of freedom and endpoint constrained by held moments; "
                 "the MLE does not exist. Use `mlht` on signed distances from this endpoint.",
                 right ? "Inf" : "0",res.nu);
    return fail("The skew Student-t likelihood is maximised as xi -> %s, i.e. by a "
               "translated half-t with %.4g degrees of freedom and endpoint at the "
               "sample %s; the MLE does not exist. Use `mlht` on signed distances from this endpoint.",
               right ? "Inf" : "0", res.nu, right ? "minimum" : "maximum");
  }
  if (res.status == cppunivariate::SstdStatus::DfTwoBoundary) {
    return fail("The skew Student-t likelihood is maximised as nu -> 2, i.e. by a "
               "two-piece t with 2 degrees of freedom (infinite variance); the MLE "
               "does not exist in the mean/sd parameterisation.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu) || !std::isfinite(res.xi)) {
    return fail("Skew Student-t MLE failed to converge (check inputs)");
  }
  if(res.status==cppunivariate::SstdStatus::BfgsFailure)
    return fail("Skew fit optimizer failed its free-coordinate score check; increase `iterlim`.");
  std::vector<std::string> fixed;
  if(mean.supplied) fixed.push_back("mean");
  if(sd.supplied) fixed.push_back("sd");
  if(nu.supplied) fixed.push_back("nu");
  if(xi.supplied) fixed.push_back("xi");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",res.mu},{"sd",res.sigma},{"nu",res.nu},{"xi",res.xi}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
