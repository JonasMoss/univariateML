#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/pig/mle.hpp"
namespace umvl_binding {
inline FitResult fit_pig(cppunivariate::ispan x, Held mean_arg = {}, Held shape_arg = {}, double reltol=1e-10, int iterlim=200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held shape = shape_arg;

 if ((mean.supplied && (!std::isfinite(mean.value) || !(mean.value>0))) ||
     (shape.supplied && (!std::isfinite(shape.value) || !(shape.value>=0))))
   return invalid_argument(fail("Fixed mean must be positive, shape nonnegative, and both finite."));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 auto r=cppunivariate::pig_mle(x,reltol,iterlim,
   mean.supplied?mean.value:nan,
   shape.supplied?shape.value:nan);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    if (r.status == cppunivariate::PigStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(r.status));
    return with_outcome(std::move(out), o, static_cast<int>(r.status));
  };
  if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("pig numerical accuracy failure: requested coefficient accuracy cannot be established."),r.stop_detail,outcome_kind(o),{"mean","shape"}));
  if (r.status==cppunivariate::PigStatus::AccuracyFailure)
    return done(stop_failure(fail("Pig requested coefficient accuracy cannot be represented after rescaling."),r.stop_detail,outcome_kind(o),{"mean","shape"}));
 if (r.status==cppunivariate::PigStatus::PoissonLimit) {
   if (mean.supplied)
     return done(stop_failure(fail("The PIG likelihood is maximised at the Poisson limit with the supplied mean fixed. Use `mlpois` for an unrestricted Poisson fit."),r.stop_detail,outcome_kind(o),{"mean","shape"}));
   return done(stop_failure(fail("The PIG likelihood is maximised at the Poisson limit. Use `mlpois` instead."),r.stop_detail,outcome_kind(o),{"mean","shape"}));
 }
 if (r.status==cppunivariate::PigStatus::ZeroLimit)
   return done(stop_failure(fail("All-zero data have an unattained point-mass supremum. Use `mlpois` for the zero point mass."),r.stop_detail,outcome_kind(o),{"mean","shape"}));
 if (r.status==cppunivariate::PigStatus::InvalidInput)
   return done(fail("PIG requires nonnegative integer data, positive finite fixed parameters, reltol > 0 and iterlim >= 1."));
 if (r.status==cppunivariate::PigStatus::SearchFailure) {
   if(r.stop_detail.budget_exhausted)
     return done(stop_failure(fail("PIG certificate exhausted its count budget; increase iterlim."),r.stop_detail,outcome_kind(o),{"mean","shape"}));
   if(r.stop_detail.comparison_tie)
     return done(stop_failure(fail("PIG Poisson-boundary score is numerically unresolved; relax the comparison or inspect the data."),r.stop_detail,"tie",{"mean","shape"}));
   return done(stop_failure(fail("PIG scalar search did not converge; increase iterlim or inspect the parameter range."),r.stop_detail,outcome_kind(o),{"mean","shape"}));
 }
 std::vector<std::string> fixed;
 if (mean.supplied) fixed.push_back("mean");
 if (shape.supplied) fixed.push_back("shape");
 return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",r.mean},{"shape",r.shape}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
