#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/st/mle.hpp"
namespace umvl_binding {
inline FitResult fit_st(cppunivariate::span x,Held xi_arg={},Held omega_arg={},Held alpha_arg={},Held nu_arg={},double reltol=1e-6,int iterlim=200) {
 const auto fail=[](const std::string& s){return failure(s);};
 const double nan=cppunivariate::special::kNaN;
 const double xi=xi_arg.supplied?xi_arg.value:nan,omega=omega_arg.supplied?omega_arg.value:nan,alpha=alpha_arg.supplied?alpha_arg.value:nan,nu=nu_arg.supplied?nu_arg.value:nan;
 if((xi_arg.supplied&&!std::isfinite(xi))||(omega_arg.supplied&&(!std::isfinite(omega)||omega<=0))||(alpha_arg.supplied&&!std::isfinite(alpha))||(nu_arg.supplied&&(!std::isfinite(nu)||nu<=0)))return fail("Held xi and alpha must be finite; omega and nu must be finite and positive.");
 const auto r=cppunivariate::st_mle(x,reltol,iterlim,xi,omega,alpha,nu);
 switch(r.status){
 case cppunivariate::StStatus::InvalidInput:return fail("Skew-t fitting requires finite data and valid parameters.");
 case cppunivariate::StStatus::Unbounded:return fail("Skew-t likelihood is unbounded through scale collapse at a tied observation.");
 case cppunivariate::StStatus::ScaleBoundary:return fail("Skew-t scale collapse is at equality; a regular maximum is not resolved.");
 case cppunivariate::StStatus::NumericalFailure:
  if(r.reason==1)return fail("Skew-t location-scale profiles failed the score check.");
  if(r.reason==2)return fail("Skew-normal limiting-face comparison failed.");
  if(r.reason==3)return fail("Half-t limiting-face comparison failed.");
  if(r.reason==4)return fail("Skew-t joint search did not resolve a finite maximum above the profiled candidates.");
  return fail("Skew-t numerical search reached an unresolved limiting region.");
 case cppunivariate::StStatus::SkewNormalBoundary:return fail("The degrees of freedom escape to infinity; use `mlsn`.");
 case cppunivariate::StStatus::RightHalfTBoundary:return fail("The shape escapes to infinity; use `mlht`.");
 case cppunivariate::StStatus::LeftHalfTBoundary:return fail("The shape escapes to minus infinity; use `mlht` on `-x`.");
 case cppunivariate::StStatus::RightHalfNormalBoundary:return fail("The shape and degrees of freedom escape to infinity; use `mlhnorm`.");
 case cppunivariate::StStatus::LeftHalfNormalBoundary:return fail("The shape and degrees of freedom escape jointly; use `mlhnorm` on `-x`.");
 case cppunivariate::StStatus::Ok:break;
 }
 FitResult out;out.estimates={{"xi",r.xi},{"omega",r.omega},{"alpha",r.alpha},{"nu",r.nu}};out.logLik=r.logLik;out.iterations=r.iterations;out.has_iterations=true;out.has_fixed=true;
 if(xi_arg.supplied)out.fixed.push_back("xi");if(omega_arg.supplied)out.fixed.push_back("omega");if(alpha_arg.supplied)out.fixed.push_back("alpha");if(nu_arg.supplied)out.fixed.push_back("nu");
 out.has_restricted=true;if(r.restricted&64u)out.restricted.emplace_back("nu","lower");return out;
}
}
