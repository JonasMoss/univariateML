#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/genbeta/mle.hpp"
namespace umvl_binding {
inline FitResult fit_genbeta(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held shape3_arg = {}, Held scale_arg = {}, double tol = 1e-10, int iterlim = 300) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held shape3 = shape3_arg;
  const double scale = scale_arg.supplied?scale_arg.value:NAN;

  if(!(tol>0)||!std::isfinite(tol)||iterlim<1)
    return invalid_argument(fail("`tol` must be positive and finite and `iterlim` must be positive."));
  std::vector<std::string> fixed;
    double checked_value_0 = std::numeric_limits<double>::quiet_NaN();
  if (shape1.supplied) {
const Held v = shape1;
const std::string name = "shape1";

    const double z=v.value;
    if(!(z>0)||!std::isfinite(z))return invalid_argument(fail("`%s` must be a positive finite number.",name));
    fixed.push_back(name);checked_value_0 = z;

  }
  double checked_value_1 = std::numeric_limits<double>::quiet_NaN();
  if (shape2.supplied) {
const Held v = shape2;
const std::string name = "shape2";

    const double z=v.value;
    if(!(z>0)||!std::isfinite(z))return invalid_argument(fail("`%s` must be a positive finite number.",name));
    fixed.push_back(name);checked_value_1 = z;

  }
  double checked_value_2 = std::numeric_limits<double>::quiet_NaN();
  if (shape3.supplied) {
const Held v = shape3;
const std::string name = "shape3";

    const double z=v.value;
    if(!(z>0)||!std::isfinite(z))return invalid_argument(fail("`%s` must be a positive finite number.",name));
    fixed.push_back(name);checked_value_2 = z;

  }

  const double a=checked_value_0,b=checked_value_1,t=checked_value_2;
  fixed.push_back("scale");
  const auto r=cppunivariate::genbeta_mle(x,scale,tol,iterlim,a,b,t);
  if(r.product_ridge)fixed.insert(fixed.end()-1,"shape3");
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(r.status));
    return with_outcome(std::move(out), o, static_cast<int>(r.status),{{"shape1",shape1_arg.supplied},{"shape2",shape2_arg.supplied},{"shape3",shape3_arg.supplied},{"scale",scale_arg.supplied}});
  };
  if(r.status==cppunivariate::GenbetaStatus::Unsupported)return done(failure("Estimating scale is not supported. The unrestricted likelihood is unbounded when shape2 is free or below one. With other held shapes, a finite MLE may exist but is not computed. Supply scale."));


  if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("genbeta numerical accuracy failure: requested coefficient accuracy cannot be established."),r.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale"}));
  if(r.stop_detail.budget_exhausted)return done(stop_failure(fail("Generalized beta certificate budget exhausted; increase iterlim."),r.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale"}));
  if(r.stop_detail.comparison_tie)return done(comparison_payload(stop_failure(fail("Generalized beta search unresolved; increase iterlim or hold shapes."),r.stop_detail,PayloadKind::Comparison,{"shape1","shape2","shape3","scale"}),o,cppunivariate::comparison_choice(r),{{"shape1",shape1_arg},{"shape2",shape2_arg},{"shape3",shape3_arg},{"scale",scale_arg}}));
  switch(r.status) {
    case cppunivariate::GenbetaStatus::Ok:break;
    case cppunivariate::GenbetaStatus::UnitGammaBoundary:
      if(!std::isnan(b))return done(stop_failure(fail("Generalized beta selects the unit-gamma limit with shapelog held at shape2. `mllgamma(scale / x)` fits the unrestricted loggamma; holding shapelog there is not supported."),r.stop_detail,outcome_payload(o),{"shape","scale","upper"},"unavailable:unit-gamma"));
      return done(stop_failure(fail(r.allfree_certified?"Generalized beta selects the unit-gamma limit. Use `mllgamma(scale / x)`.":"Generalized beta selects the unit-gamma limit (uncertified global comparison). Use `mllgamma(scale / x)`."),r.stop_detail,outcome_payload(o),{"shape","scale","upper"},"unavailable:unit-gamma"));
    case cppunivariate::GenbetaStatus::PowerBoundary:
      if(!std::isnan(b))return done(stop_failure(fail("Generalized beta selects power with the endpoint held: use `mlgenbeta(x, scale = scale, shape2 = 1)`. `mlpower` estimates its endpoint."),r.stop_detail,outcome_payload(o),{"alpha","beta"},"power"));
      return done(stop_failure(fail(r.allfree_certified?"Generalized beta selects a limiting power law. Use `mlpower`.":"Generalized beta selects a limiting power law (uncertified global comparison). Use `mlpower`."),r.stop_detail,outcome_payload(o),{"alpha","beta"},"power"));
    case cppunivariate::GenbetaStatus::Unbounded:return done(stop_failure(fail("Generalized beta requires observations strictly between 0 and scale; constant samples with free beta shapes are not supported (the unrestricted likelihood is unbounded)."),{},outcome_payload(o),{}));
    case cppunivariate::GenbetaStatus::InvalidData:return done(fail("Generalized beta requires observations strictly between 0 and scale; constant samples with free beta shapes are not supported (the unrestricted likelihood is unbounded)."));
    case cppunivariate::GenbetaStatus::InvalidFixed:return done(fail("Generalized beta held parameters must be positive finite numbers."));
    case cppunivariate::GenbetaStatus::SearchUnresolved: {
      const char* message="Generalized beta search unresolved; increase iterlim or hold shapes.";
      switch(r.certificate_failure) {
        case cppunivariate::GenbetaCertificateFailure::LegacyContradicted:message="Generalized beta legacy proposal is below a validated improving witness; the global comparison remains unresolved. Increase iterlim or hold shapes.";break;
        case cppunivariate::GenbetaCertificateFailure::ShapeSolve:message="Generalized beta conditional shape solve unresolved; increase iterlim.";break;
        case cppunivariate::GenbetaCertificateFailure::ShapeSigns:message="Generalized beta conditional shape signs unresolved at floating-point precision; hold shapes.";break;
        case cppunivariate::GenbetaCertificateFailure::GammaNeighborhood:message=std::isnan(b)?"Generalized beta gamma-limit neighborhood unresolved. `mllgamma(scale / x)` gives the fitted gamma face; hold shape3 to fit a fixed-power submodel.":"Generalized beta gamma-limit neighborhood unresolved; the held-shape limit is not certified. `mllgamma` fits an unrestricted shape; hold shapes to resolve this fit.";break;
        case cppunivariate::GenbetaCertificateFailure::PowerNeighborhood:message=std::isnan(b)?"Generalized beta moving-endpoint power neighborhood unresolved; increase iterlim or hold shapes.":"Generalized beta held-endpoint power neighborhood unresolved; increase iterlim or hold shapes.";break;
        case cppunivariate::GenbetaCertificateFailure::Tail:message="Generalized beta certificate tail bound unresolved; increase iterlim or hold shapes.";break;
        case cppunivariate::GenbetaCertificateFailure::RootSigns:message="Generalized beta power-root signs unresolved at floating-point precision; hold shapes.";break;
        case cppunivariate::GenbetaCertificateFailure::CoefficientInclusion:message="Generalized beta coefficient enclosure unresolved; increase iterlim or hold shapes.";break;
        case cppunivariate::GenbetaCertificateFailure::Arithmetic:message="Generalized beta certificate arithmetic unresolved; hold shapes.";break;
        default:break;
      }
      return done(stop_failure(fail(message),r.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale"}));
    }
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",r.shape1},{"shape2",r.shape2},{"shape3",r.shape3},{"scale",r.scale}}; if(r.product_ridge)out.extra.emplace_back("identified_product",r.shape1*r.shape3); out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true;  out.extra_vectors.emplace_back("support", std::vector<double>{0,scale}); return out; }());

}
}
