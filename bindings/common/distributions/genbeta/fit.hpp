#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/genbeta/mle.hpp"
namespace umvl_binding {
inline FitResult fit_genbeta(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held shape3_arg = {}, Held scale_arg = {}, double gradtol = 1e-7, int iterlim = 300) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held shape3 = shape3_arg;
  if (!scale_arg.supplied) return failure("Estimating the threshold is not supported: endpoint singularities or unattained limit models prevent an unrestricted MLE (P0, A4).");
  const double scale = scale_arg.value;

  if(!(gradtol>0)||!std::isfinite(gradtol)||iterlim<1)
    return fail("`gradtol` must be positive and finite and `iterlim` must be positive.");
  std::vector<std::string> fixed;
    double checked_value_0 = std::numeric_limits<double>::quiet_NaN();
  if (shape1.supplied) {
const Held v = shape1;
const std::string name = "shape1";

    const double z=v.value;
    if(!(z>0)||!std::isfinite(z))return fail("`%s` must be a positive finite number.",name);
    fixed.push_back(name);checked_value_0 = z;

  }
  double checked_value_1 = std::numeric_limits<double>::quiet_NaN();
  if (shape2.supplied) {
const Held v = shape2;
const std::string name = "shape2";

    const double z=v.value;
    if(!(z>0)||!std::isfinite(z))return fail("`%s` must be a positive finite number.",name);
    fixed.push_back(name);checked_value_1 = z;

  }
  double checked_value_2 = std::numeric_limits<double>::quiet_NaN();
  if (shape3.supplied) {
const Held v = shape3;
const std::string name = "shape3";

    const double z=v.value;
    if(!(z>0)||!std::isfinite(z))return fail("`%s` must be a positive finite number.",name);
    fixed.push_back(name);checked_value_2 = z;

  }

  const double a=checked_value_0,b=checked_value_1,t=checked_value_2;
  if(b==1&&std::isnan(a)&&std::isnan(t))fixed.push_back("shape3");
  fixed.push_back("scale");
  const auto r=cppunivariate::genbeta_mle(x,scale,gradtol,iterlim,a,b,t);
  switch(r.status) {
    case cppunivariate::GenbetaStatus::Ok:break;
    case cppunivariate::GenbetaStatus::UnitGammaBoundary:
      if(!std::isnan(b))return fail("Generalized beta selects the unit-gamma limit with shapelog held at shape2. `mllgamma(scale / x)` fits the unrestricted loggamma; holding shapelog there is not supported.");
      return fail("Generalized beta selects the unit-gamma limit. Use `mllgamma(scale / x)`.");
    case cppunivariate::GenbetaStatus::PowerBoundary:
      if(!std::isnan(b))return fail("Generalized beta selects power with the endpoint held: use `mlgenbeta(x, scale = scale, shape2 = 1)`. `mlpower` estimates its endpoint.");
      return fail("Generalized beta selects a limiting power law. Use `mlpower`.");
    case cppunivariate::GenbetaStatus::InvalidData:return fail("Generalized beta requires observations strictly between 0 and scale; constant samples with free beta shapes are not supported (the unrestricted likelihood is unbounded).");
    case cppunivariate::GenbetaStatus::InvalidFixed:return fail("Generalized beta held parameters must be positive finite numbers.");
    case cppunivariate::GenbetaStatus::SearchUnresolved:return fail("Generalized beta search unresolved; increase iterlim or hold shapes.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",r.shape1},{"shape2",r.shape2},{"shape3",r.shape3},{"scale",r.scale}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; out.extra_vectors.emplace_back("support", std::vector<double>{0,scale}); return out; }();

}
}
