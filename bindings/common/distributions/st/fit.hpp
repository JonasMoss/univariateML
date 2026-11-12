#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/st/mle.hpp"
namespace umvl_binding {
inline FitResult fit_st(cppunivariate::span x,Held xi_arg={},Held omega_arg={},Held alpha_arg={},Held nu_arg={},double reltol=1e-10,int iterlim=200) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(reltol>0 && std::isfinite(reltol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
 const auto fail=[](const std::string& s){return failure(s);};
 const double nan=cppunivariate::special::kNaN;
 const double xi=xi_arg.supplied?xi_arg.value:nan,omega=omega_arg.supplied?omega_arg.value:nan,alpha=alpha_arg.supplied?alpha_arg.value:nan,nu=nu_arg.supplied?nu_arg.value:nan;
 if((xi_arg.supplied&&!std::isfinite(xi))||(omega_arg.supplied&&(!std::isfinite(omega)||omega<=0))||(alpha_arg.supplied&&!std::isfinite(alpha))||(nu_arg.supplied&&(!std::isfinite(nu)||nu<=0)))return fail("Held xi and alpha must be finite; omega and nu must be finite and positive.");
 const auto r=cppunivariate::st_mle(x,reltol,iterlim,xi,omega,alpha,nu);
  if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return stop_failure(failure("st numerical accuracy failure: requested coefficient accuracy cannot be established."),r.stop_detail,"numerical",{"xi","omega","alpha","nu"});
  if(r.accuracy_failure)return stop_failure(fail("st numerical accuracy failure: final Newton refinement did not converge to the requested coefficient tolerance; increase iterlim or inspect conditioning."),r.stop_detail,"numerical",{"xi","omega","alpha","nu"});
 if(r.stop_detail.comparison_tie || r.budget_exhausted) {
  const std::string message=r.certified&&r.stop_detail.comparison_tie?"Skew-t constant-data supremum is tied between the two limiting faces.":r.budget_exhausted?"Skew-t certificate budget exhausted; increase `iterlim`.":r.reason==2?"Skew-normal limiting-face comparison failed.":r.reason==3?"Half-t limiting-face comparison failed.":"Skew-t numerical search reached an unresolved limiting region.";
  return classify(fail(message),r.stop_detail,r.stop_detail.comparison_tie?"tie":"budget",{"xi","omega","alpha","nu"});
 }
 switch(r.status){
 case cppunivariate::StStatus::InvalidInput:return fail("Skew-t fitting requires finite data and valid parameters.");
 case cppunivariate::StStatus::Unbounded:return classify(fail("Skew-t likelihood is unbounded through scale collapse at a tied observation."),r.stop_detail,"nomle",{},"");
 case cppunivariate::StStatus::ScaleBoundary:
  if(r.reason==6)return stop_failure(fail("Skew-t likelihood has an unattained scale-collapse supremum."),r.stop_detail,"nomle",{"xi","omega","alpha","nu"});
  return fail("Skew-t scale collapse is at equality; a regular maximum is not resolved.");
 case cppunivariate::StStatus::NumericalFailure:
  if(r.reason==1)return fail("Skew-t location-scale profiles failed the score check.");
  if(r.reason==2)return fail("Skew-normal limiting-face comparison failed.");
  if(r.reason==3)return fail("Half-t limiting-face comparison failed.");
  if(r.reason==4)return fail("Skew-t joint search did not resolve a finite maximum above the profiled candidates.");
  return stop_failure(fail("Skew-t numerical search reached an unresolved limiting region."),r.stop_detail,"numerical",{"xi","omega","alpha","nu"});
 case cppunivariate::StStatus::SkewNormalBoundary:return classify(fail("The degrees of freedom escape to infinity; use `mlsn`."),r.stop_detail,"limit",{"xi","omega","alpha"},"sn");
 case cppunivariate::StStatus::RightHalfTBoundary:return classify(fail("The shape escapes to infinity; use `mlht`."),r.stop_detail,"limit",{"nu","mu","sigma"},"ht");
 case cppunivariate::StStatus::LeftHalfTBoundary:return classify(fail("The shape escapes to minus infinity; use `mlht` on `-x`."),r.stop_detail,"limit",{"nu","mu","sigma"},"unavailable:reflected-half-t");
 case cppunivariate::StStatus::RightHalfNormalBoundary:return classify(fail("The shape and degrees of freedom escape to infinity; use `mlhnorm`."),r.stop_detail,"limit",{"mu","sigma"},"hnorm");
 case cppunivariate::StStatus::LeftHalfNormalBoundary:return classify(fail("The shape and degrees of freedom escape jointly; use `mlhnorm` on `-x`."),r.stop_detail,"limit",{"mu","sigma"},"unavailable:reflected-half-normal");
 case cppunivariate::StStatus::Ok:break;
 }
 FitResult out;out.estimates={{"xi",r.xi},{"omega",r.omega},{"alpha",r.alpha},{"nu",r.nu}};out.logLik=r.logLik;out.iterations=r.iterations;out.has_iterations=true;out.has_fixed=true;
 if(xi_arg.supplied)out.fixed.push_back("xi");if(omega_arg.supplied)out.fixed.push_back("omega");if(alpha_arg.supplied)out.fixed.push_back("alpha");if(nu_arg.supplied)out.fixed.push_back("nu");
 out.has_restricted=true;if(r.restricted&64u)out.restricted.emplace_back("nu","lower");return out;
}
}
