#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/genbeta/mle.hpp"
namespace umvl_binding {
inline FitResult fit_genbeta(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held shape3_arg = {}, Held scale_arg = {}, double gradtol = 1e-7, int iterlim = 300) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(gradtol>0 && std::isfinite(gradtol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held shape3 = shape3_arg;
  if (!scale_arg.supplied) return failure("Estimating scale is not supported. The unrestricted likelihood is unbounded when shape2 is free or below one. With other held shapes, a finite MLE may exist but is not computed. Supply scale.");
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
  if(r.stop_detail.comparison_tie)return classify(fail("Generalized beta search unresolved; increase iterlim or hold shapes."),r.stop_detail,"tie",{"shape1","shape2","shape3","scale"});
  switch(r.status) {
    case cppunivariate::GenbetaStatus::Ok:break;
    case cppunivariate::GenbetaStatus::UnitGammaBoundary:
      if(!std::isnan(b))return classify(fail("Generalized beta selects the unit-gamma limit with shapelog held at shape2. `mllgamma(scale / x)` fits the unrestricted loggamma; holding shapelog there is not supported."),r.stop_detail,"limit",{"shape","scale","upper"},"unavailable:unit-gamma");
      return classify(fail("Generalized beta selects the unit-gamma limit. Use `mllgamma(scale / x)`."),r.stop_detail,"limit",{"shape","scale","upper"},"unavailable:unit-gamma");
    case cppunivariate::GenbetaStatus::PowerBoundary:
      if(!std::isnan(b))return classify(fail("Generalized beta selects power with the endpoint held: use `mlgenbeta(x, scale = scale, shape2 = 1)`. `mlpower` estimates its endpoint."),r.stop_detail,"limit",{"alpha","beta"},"power");
      return classify(fail("Generalized beta selects a limiting power law. Use `mlpower`."),r.stop_detail,"limit",{"alpha","beta"},"power");
    case cppunivariate::GenbetaStatus::Unbounded:return classify(fail("Generalized beta requires observations strictly between 0 and scale; constant samples with free beta shapes are not supported (the unrestricted likelihood is unbounded)."),{},"nomle",{});
    case cppunivariate::GenbetaStatus::InvalidData:return fail("Generalized beta requires observations strictly between 0 and scale; constant samples with free beta shapes are not supported (the unrestricted likelihood is unbounded).");
    case cppunivariate::GenbetaStatus::InvalidFixed:return fail("Generalized beta held parameters must be positive finite numbers.");
    case cppunivariate::GenbetaStatus::SearchUnresolved:return fail("Generalized beta search unresolved; increase iterlim or hold shapes.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",r.shape1},{"shape2",r.shape2},{"shape3",r.shape3},{"scale",r.scale}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; out.extra_vectors.emplace_back("support", std::vector<double>{0,scale}); return out; }();

}
}
