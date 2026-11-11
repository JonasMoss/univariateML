#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/betapr/mle.hpp"
#include <cmath>
namespace umvl_binding {
inline FitResult fit_betapr(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held scale_arg = {}, double gradtol = 1e-6, int iterlim = 2000, double shape10 = 0, double shape20 = 0, double scale0 = 0, double reltol = 1e-13, double abstol = 1e-10) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& message) { return failure(message, warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held scale = scale_arg;

  const double held = scale.supplied ? scale.value : 1.0;
  if (!(held > 0) || !std::isfinite(held))
    return fail("`scale` must be a positive finite number.");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=shape1.supplied?shape1.value:nan;
  const double b=shape2.supplied?shape2.value:nan;
  if((shape1.supplied&&(!(a>0)||!std::isfinite(a))) ||
     (shape2.supplied&&(!(b>0)||!std::isfinite(b))))
    return fail("Held `shape1` and `shape2` must be positive finite numbers.");
  auto res = (shape1.supplied||shape2.supplied)
      ? cppunivariate::betapr_mle_holds(x,a,b,scale.supplied?held:nan,gradtol,iterlim)
      : scale.supplied
      ? cppunivariate::betapr_mle_fixed_scale(x, held, gradtol, iterlim)
      : cppunivariate::betapr_mle(x, gradtol, iterlim,
                                      shape10, shape20, scale0, reltol, abstol);
  if(res.status==cppunivariate::BetaPrimeStatus::ExpBoundary)
    return stop_failure(fail("The certified beta-prime submodel selects the exponential limit. Use `mlexp`."),res.stop_detail,"limit",{"shape1","shape2","scale"},"exp");
  if(res.status==cppunivariate::BetaPrimeStatus::InvExpBoundary)
    return stop_failure(fail("The certified beta-prime submodel selects the inverse exponential limit. Use `mlinvexp`."),res.stop_detail,"limit",{"shape1","shape2","scale"},"invexp");
  if (res.status == cppunivariate::BetaPrimeStatus::GammaBoundary && res.stop_detail.limit_coef.empty())
    return fail("The beta-prime likelihood selects the gamma limit. Use `mlgamma` to fit this model.");
  if (res.status == cppunivariate::BetaPrimeStatus::InvGammaBoundary && res.stop_detail.limit_coef.empty())
    return fail("The beta-prime likelihood selects the inverse-gamma limit. Use `mlinvgamma` to fit this model.");
  if (res.status == cppunivariate::BetaPrimeStatus::GammaBoundary)
    return stop_failure(fail("The beta-prime likelihood selects the gamma limit. Use `mlgamma` to fit this model."),res.stop_detail,"limit",{"shape1","shape2","scale"},"gamma");
  if (res.status == cppunivariate::BetaPrimeStatus::InvGammaBoundary)
    return stop_failure(fail("The beta-prime likelihood selects the inverse-gamma limit. Use `mlinvgamma` to fit this model."),res.stop_detail,"limit",{"shape1","shape2","scale"},"invgamma");
  if (res.status == cppunivariate::BetaPrimeStatus::BfgsFailure && res.certificate_failure==cppunivariate::LomaxCertificateFailure::Budget)
    return stop_failure(fail("Beta-prime BfgsFailure: global certificate budget exhausted (128 * min(iterlim, 100000) evaluations); raising iterlim may resolve this stop."),res.stop_detail,"budget",{"shape1","shape2","scale"},"");
  if (res.status == cppunivariate::BetaPrimeStatus::BfgsFailure)
    return stop_failure(fail("Beta-prime optimizer BfgsFailure: profile did not converge or its global certificate is unresolved; increase iterlim. An unresolved gamma or inverse-gamma comparison does not select `mlgamma` or `mlinvgamma`."),res.stop_detail,res.certificate_failure==cppunivariate::LomaxCertificateFailure::Budget?"budget":res.stop_detail.comparison_tie?"tie":res.certificate_failure==cppunivariate::LomaxCertificateFailure::Arithmetic?"numerical":"unresolved",{"shape1","shape2","scale"},"");
  if(res.status==cppunivariate::BetaPrimeStatus::InvalidStart && !shape1.supplied && !shape2.supplied && !x.empty() &&
      std::all_of(x.begin(),x.end(),[&](double v){return v>0&&std::isfinite(v)&&v==x[0];}))
    return stop_failure(fail("Beta-prime finite MLEs require positive log-variance; constant positive observations have unbounded likelihood."),res.stop_detail,"nomle",{"shape1","shape2","scale"},"");
  if (res.status == cppunivariate::BetaPrimeStatus::InvalidStart ||
      !std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.scale))
    return fail("Beta-prime MLE requires positive finite observations with positive log-variance.");
  std::vector<std::string> fixed;
  if(shape1.supplied)fixed.push_back("shape1");
  if(shape2.supplied)fixed.push_back("shape2");
  if(scale.supplied)fixed.push_back("scale");
  return [&]() { FitResult out; out.extra.emplace_back("certified",res.certified?1.:0.); out.warnings = warnings; out.estimates = {{"shape1",res.alpha},{"shape2",res.beta},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
