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
  if(mu_arg.supplied && !std::isfinite(mu))return fail("Held `mu` must be finite.");
  if(sigma_arg.supplied && (!(sigma>0)||!std::isfinite(sigma)))return fail("Held `sigma` must be positive and finite.");
  if(Q_arg.supplied && !std::isfinite(q))return fail("Held `Q` must be finite.");
  if(!(tol>0) || !std::isfinite(tol) || iterlim<1)return fail("`tol` and `iterlim` must be positive and finite.");
  const auto fit=cppunivariate::gengamma_mle(x,tol,iterlim,mu,sigma,q);
  if(fit.status==cppunivariate::GengammaStatus::InvalidData)return fail("gengamma requires nonempty positive finite data.");
  if(fit.status==cppunivariate::GengammaStatus::InvalidFixed)return fail("Invalid held gengamma coefficients.");
  if(fit.status==cppunivariate::GengammaStatus::Unbounded)
    return stop_failure(fail("The gengamma likelihood is unbounded as scale shrinks to zero."),{},"nomle",{});
  if(fit.status==cppunivariate::GengammaStatus::PowerBoundary) {
    auto result=fail("The gengamma likelihood has a power limit. Use `mlpower`.");
    result.extra.push_back({"certified",1});
    return stop_failure(result,fit.stop_detail,"limit",{"alpha","beta"},"power");
  }
  if(fit.status==cppunivariate::GengammaStatus::ParetoBoundary) {
    auto result=fail("The gengamma likelihood has a Pareto limit. Use `mlpareto`.");
    result.extra.push_back({"certified",1});
    return stop_failure(result,fit.stop_detail,"limit",{"a","b"},"pareto");
  }
  if(fit.status!=cppunivariate::GengammaStatus::Ok)
    return stop_failure(fail("The gengamma likelihood search failed its numerical accuracy or candidate comparison."),fit.stop_detail,"numerical",{"mu","sigma","Q"});
  if(!(std::isfinite(fit.mu)&&std::isfinite(fit.sigma)&&fit.sigma>0&&std::isfinite(fit.Q)&&std::isfinite(fit.logLik)))return fail("The gengamma fit is not representable.");
  FitResult out;
  out.estimates={{"mu",fit.mu},{"sigma",fit.sigma},{"Q",fit.Q}};
  out.logLik=fit.logLik;out.iterations=fit.iterations;out.has_iterations=true;
  if(mu_arg.supplied)out.fixed.push_back("mu");
  if(sigma_arg.supplied)out.fixed.push_back("sigma");
  if(Q_arg.supplied)out.fixed.push_back("Q");
  out.has_fixed=true;out.extra.push_back({"certified",fit.certified?1.0:0.0});
  return out;
}
}
