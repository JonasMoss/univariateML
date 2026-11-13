#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/paralogis/mle.hpp"
#include <cmath>
#include <algorithm>
#include <limits>
namespace umvl_binding {
inline FitResult fit_paralogis(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, Held shape0 = {}, double gradtol = 1.220703e-4, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape = shape_arg;
  const Held scale = scale_arg;

 if(shape.supplied && (!(shape.value>0)||!std::isfinite(shape.value))) return invalid_argument(fail("`shape` must be positive and finite."));

  double start = std::numeric_limits<double>::quiet_NaN();
  if (shape0.supplied) {
    start = shape0.value;
    if (!(start > 0.0) || !std::isfinite(start)) {
      return invalid_argument(fail("`shape0` must be a positive finite number."));
    }
  }
  const double held = scale.supplied ? scale.value : 1.0;
  if (!(held > 0) || !std::isfinite(held))
    return invalid_argument(fail("`scale` must be a positive finite number."));
  auto res = shape.supplied ? cppunivariate::paralogis_mle_holds(x,shape.value,scale.supplied?held:NAN,gradtol,iterlim) : scale.supplied
      ? cppunivariate::paralogis_mle_fixed_scale(x, held, gradtol, iterlim, start)
      : cppunivariate::paralogis_mle(x, gradtol, iterlim, start);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };

  if(res.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("paralogis numerical accuracy failure: requested coefficient accuracy cannot be established."),res.stop_detail,outcome_kind(o),{"shape","scale"}));
  if(res.status==cppunivariate::ParalogisStatus::Unbounded)
    return done(stop_failure(fail("The paralogis maximum likelihood estimator does not exist for constant data: the likelihood is unbounded."),res.stop_detail,outcome_kind(o),{"shape","scale"}));
  if(res.status==cppunivariate::ParalogisStatus::CertificateBudget)
    return done(stop_failure(fail("Paralogis certificate budget exhausted; no verified MLE. Raising iterlim may resolve this stop."),res.stop_detail,outcome_kind(o),{"shape","scale"}));
  if(res.status==cppunivariate::ParalogisStatus::NumericalFailure)
    return done(stop_failure(fail("Paralogis certificate numerical failure: unresolved bounds or fitted parameters exceed double precision; no verified MLE."),res.stop_detail,outcome_kind(o),{"shape","scale"}));
  if(res.status==cppunivariate::ParalogisStatus::InvalidStart)
    return done(fail("Paralogis MLE requires positive, finite observations."));
  if(!std::isfinite(res.shape)||!std::isfinite(res.scale)||!std::isfinite(res.logLik))
    return done(stop_failure(fail("Paralogis MLE failed: fitted parameters or likelihood exceed double precision"),res.stop_detail,"numerical",{"shape","scale"}));
  if(res.status==cppunivariate::ParalogisStatus::BfgsFailure)
    return done(stop_failure(fail("Paralogis optimizer BfgsFailure: proposal failed to converge; no verified MLE; increase iterlim or change shape0."),res.stop_detail,outcome_kind(o),{"shape","scale"}));
  std::vector<std::string> fixed;
  if(shape.supplied) fixed.push_back("shape");
  if(scale.supplied) fixed.push_back("scale");
  return done([&]() { FitResult out; out.extra.emplace_back("certified",res.certified?1.:0.); out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
