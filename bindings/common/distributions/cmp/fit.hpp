#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/cmp/mle.hpp"
namespace umvl_binding {
inline FitResult fit_cmp(cppunivariate::span x, Held lambda_arg = {}, Held nu_arg = {}, double reltol=1e-10, int iterlim=100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held lambda = lambda_arg;
  const Held nu = nu_arg;

 cppunivariate::CmpFitControl ctl; ctl.tol=reltol; ctl.maxit=iterlim;
 if (lambda.supplied) {ctl.fixed_lambda=lambda.value;
  if (!std::isfinite(ctl.fixed_lambda) || ctl.fixed_lambda<0) return fail("fixed lambda must be nonnegative and finite.");}
 if (nu.supplied) {ctl.fixed_nu=nu.value;
  if (!std::isfinite(ctl.fixed_nu) || ctl.fixed_nu<0) return fail("fixed nu must be nonnegative and finite.");}
 const auto res=cppunivariate::cmp_domain_mle(x,ctl);
  std::string stop_kind;
  switch(res.status) {
    case cppunivariate::CmpStatus::GeometricLimit: stop_kind="limit";break;
    case cppunivariate::CmpStatus::TwoPointLimit: stop_kind="limit";break;
    case cppunivariate::CmpStatus::PointMassLimit: stop_kind="limit";break;
    default:break;
  }
  if(res.status==cppunivariate::CmpStatus::IterationFailure || res.status==cppunivariate::CmpStatus::WorkLimit)stop_kind="numerical";
  if(res.stop_detail.budget_exhausted)stop_kind="budget";
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,res.stop_detail,stop_kind,{"lambda","nu"});
  };

 switch(res.status) {
 case cppunivariate::CmpStatus::Ok: break;
 case cppunivariate::CmpStatus::GeometricLimit: return stopped_fail("The MLE approaches the geometric face; use mlgeom.");
 case cppunivariate::CmpStatus::TwoPointLimit: return stopped_fail("The MLE approaches an adjacent two-point law.");
 case cppunivariate::CmpStatus::PointMassLimit: return stopped_fail("The MLE approaches a point mass.");
 case cppunivariate::CmpStatus::InvalidInput: return stopped_fail("CMP requires nonnegative integer counts at most 2147483647, valid fixed parameters and positive controls.");
 default:
   if(res.stop_detail.budget_exhausted)return stopped_fail("CMP acceptance exhausted its work budget. Raising iterlim may resolve this stop.");
   return stopped_fail("CMP calculation failed to converge or exceeded its work limit.");
 }
 std::vector<std::string> fixed;
 if(lambda.supplied) fixed.push_back("lambda");
 if(nu.supplied) fixed.push_back("nu");
 return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",lambda.supplied?ctl.fixed_lambda:std::exp(res.eta)},{"nu",res.nu}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
