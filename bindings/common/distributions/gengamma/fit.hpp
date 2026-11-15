#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gengamma/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_gengamma(cppunivariate::span x,Held mu_arg={},Held sigma_arg={},Held Q_arg={},double tol=1e-10,int iterlim=100) {
  const auto fail=[](const std::string& message){return failure(message,{});};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double mu=mu_arg.supplied?mu_arg.value:nan;
  const double sigma=sigma_arg.supplied?sigma_arg.value:nan;
  const double q=Q_arg.supplied?Q_arg.value:nan;
  if(mu_arg.supplied && !std::isfinite(mu))return invalid_argument(fail("Held `mu` must be finite."));
  if(sigma_arg.supplied && (!(sigma>0)||!std::isfinite(sigma)))return invalid_argument(fail("Held `sigma` must be positive and finite."));
  if(Q_arg.supplied && !std::isfinite(q))return invalid_argument(fail("Held `Q` must be finite."));
  if(!(tol>0) || !std::isfinite(tol) || iterlim<1)return invalid_argument(fail("`tol` and `iterlim` must be positive and finite."));
  const auto fit=cppunivariate::gengamma_mle(x,tol,iterlim,mu,sigma,q);
  const auto o = cppunivariate::outcome(fit);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(fit.status),{{"mu",mu_arg.supplied},{"sigma",sigma_arg.supplied},{"Q",Q_arg.supplied}});
  };
  if(fit.status==cppunivariate::GengammaStatus::InvalidData)return done(fail("gengamma requires nonempty positive finite data."));
  if(fit.status==cppunivariate::GengammaStatus::InvalidFixed)return done(fail("Invalid held gengamma coefficients."));
  if(fit.status==cppunivariate::GengammaStatus::Unbounded)
    return done(stop_failure(fail("The gengamma likelihood is unbounded as scale shrinks to zero."),{},outcome_payload(o),{}));
  if(fit.status==cppunivariate::GengammaStatus::PowerBoundary) {
    auto result=fail("The gengamma likelihood has a power limit. Use `mlpower`.");

    return done(stop_failure(result,fit.stop_detail,outcome_payload(o),{"alpha","beta"},"power"));
  }
  if(fit.status==cppunivariate::GengammaStatus::ParetoBoundary) {
    auto result=fail("The gengamma likelihood has a Pareto limit. Use `mlpareto`.");

    return done(stop_failure(result,fit.stop_detail,outcome_payload(o),{"a","b"},"pareto"));
  }
  if(fit.status!=cppunivariate::GengammaStatus::Ok)
    return done(stop_failure(fail(fit.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None?
      "The gengamma fit could not meet the requested coefficient accuracy in floating-point arithmetic.":
      "The gengamma likelihood search failed its numerical accuracy or candidate comparison."),fit.stop_detail,outcome_payload(o),{"mu","sigma","Q"}));
  FitResult out;
  out.estimates={{"mu",fit.mu},{"sigma",fit.sigma},{"Q",fit.Q}};
  out.logLik=fit.logLik;out.iterations=fit.iterations;out.has_iterations=true;
  if(mu_arg.supplied)out.fixed.push_back("mu");
  if(sigma_arg.supplied)out.fixed.push_back("sigma");
  if(Q_arg.supplied)out.fixed.push_back("Q");
  out.has_fixed=true;
  return done(out);
}
}
