#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
namespace umvl_binding {
inline FitResult fit_gamma(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(abstol>0 && std::isfinite(abstol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };

  if (shape_arg.supplied && (!(shape_arg.value > 0) || !std::isfinite(shape_arg.value))) return invalid_argument(failure("Held `shape` must be positive and finite."));
  double shape=shape_arg.supplied ? shape_arg.value : NAN;
  if (scale_arg.supplied && (!(scale_arg.value > 0) || !std::isfinite(scale_arg.value))) return invalid_argument(failure("Held `scale` must be positive and finite."));
  double scale=scale_arg.supplied ? scale_arg.value : NAN;
  auto r=cppunivariate::gamma_mle(cppunivariate::span{x.data(),x.size()},abstol,iterlim,shape,scale);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) { return with_outcome(std::move(out), o, static_cast<int>(r.status)); };
  if (r.status == cppunivariate::GammaStatus::InvalidInput) {
    // Retain the empty-input diagnostic exposed by the existing JS API.
    if (x.empty()) return done(failure("Gamma MLE failed"));
    return done(failure("Gamma requires nonempty positive finite data and valid held parameters."));
  }
  if(r.status==cppunivariate::GammaStatus::TransformFailure) return done(failure("Gamma transformed observations exceed double precision."));
  if(r.status==cppunivariate::GammaStatus::IterationFailure) return done(failure("Gamma delegate optimizer IterationFailure; increase iterlim."));
  if(r.status==cppunivariate::GammaStatus::Degenerate) return done(classify(failure("The gamma maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as the shape grows."),{},outcome_kind(o),{},""));
  if(!std::isfinite(r.shape)||!std::isfinite(r.scale)||!std::isfinite(r.logLik)) return overridden(failure("Gamma MLE failed"),static_cast<int>(r.status));
  FitResult out;
  out.estimates.emplace_back("shape",r.shape); if(shape_arg.supplied) out.fixed.push_back("shape");
  out.estimates.emplace_back("scale",r.scale); if(scale_arg.supplied) out.fixed.push_back("scale");
  out.logLik=r.logLik; out.iterations=r.iterations; return done(out);
}
}
