#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/expweibull/mle.hpp"
namespace umvl_binding {
inline FitResult fit_expweibull(cppunivariate::span x,Held scale_arg={},Held shape_arg={},Held exponent_arg={},double tol=1e-10,int iterlim=100) {
  const auto fail=[](const std::string& s){return failure(s,{});};
  for(const auto& h:{scale_arg,shape_arg,exponent_arg})
    if(h.supplied && (!(h.value>0)||!std::isfinite(h.value)))return fail("Held scale, shape and exponent must be positive and finite.");
  if(!(tol>0)||!std::isfinite(tol)||iterlim<1)return fail("`tol` and `iterlim` must be positive and finite.");
  const auto r=cppunivariate::expweibull_mle(x,tol,iterlim,
    scale_arg.supplied?scale_arg.value:NAN,shape_arg.supplied?shape_arg.value:NAN,exponent_arg.supplied?exponent_arg.value:NAN);
  if(r.status==cppunivariate::ExpweibullStatus::InvalidData)return fail("expweibull requires nonempty strictly positive finite data.");
  if(r.status==cppunivariate::ExpweibullStatus::InvalidFixed)return fail("Invalid held expweibull coefficients.");
  if(r.status==cppunivariate::ExpweibullStatus::Unbounded)
    return stop_failure(fail("The expweibull likelihood is unbounded on a concentration path."),{},"nomle",{});
  if(r.status==cppunivariate::ExpweibullStatus::PowerBoundary || r.status==cppunivariate::ExpweibullStatus::PowerBoundaryTied) {
    const bool tied=r.status==cppunivariate::ExpweibullStatus::PowerBoundaryTied;
    auto out=fail(tied?"The expweibull supremum is a power envelope with endpoint ties; its likelihood differs from mlpower.":"The expweibull supremum is a power limit. Use `mlpower` with endpoint held at scale.");
    out.extra.push_back({"certified",1});
    const std::string model=tied?"unavailable:power-envelope-endpoint-ties":"power";
    auto result=stop_failure(out,r.stop_detail,"limit",{"alpha","beta"},model);
    if(tied)for(auto& candidate:result.candidates)candidate.model=model;
    return result;
  }
  if(r.status!=cppunivariate::ExpweibullStatus::Ok) {
    const std::string message=r.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::Tolerance?
      "The expweibull fit cannot attain the requested coefficient tolerance.":
      r.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::Conditioning?
      "The expweibull likelihood is too ill-conditioned at the requested coefficient accuracy.":
      r.status==cppunivariate::ExpweibullStatus::AccuracyFailure?
      "The expweibull fit cannot restore accurate representable coefficients and likelihood.":
      std::isfinite(r.stop_detail.limit_loglik)?
      "A reachable power or inverse-Weibull face refutes the expweibull finite candidate; global face ordering is unresolved.":
      "The expweibull likelihood search failed to find a locally verified maximum.";
    return stop_failure(fail(message),r.stop_detail,"numerical",{"scale","shape","exponent"});
  }
  FitResult out;out.estimates={{"scale",r.scale},{"shape",r.shape},{"exponent",r.exponent}};
  out.logLik=r.logLik;out.iterations=r.iterations;out.has_iterations=true;out.has_fixed=true;
  if(scale_arg.supplied)out.fixed.push_back("scale");
  if(shape_arg.supplied)out.fixed.push_back("shape");
  if(exponent_arg.supplied)out.fixed.push_back("exponent");
  out.extra.push_back({"certified",r.certified?1.:0.});return out;
}
}
