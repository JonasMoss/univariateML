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
  if (!std::isfinite(ctl.fixed_lambda) || ctl.fixed_lambda<0) return invalid_argument(fail("fixed lambda must be nonnegative and finite."));}
 if (nu.supplied) {ctl.fixed_nu=nu.value;
  if (!std::isfinite(ctl.fixed_nu) || ctl.fixed_nu<0) return invalid_argument(fail("fixed nu must be nonnegative and finite."));}
 const auto res=[&]() {
   if(lambda.supplied && nu.supplied) {
     // No parameters are being fitted. Match the fixed special-function cap
     // used by d/p/q, including the R fully-held diagnostic fallback.
     std::uint64_t used=0;
     cppunivariate::CmpWorkLedger ledger{used,1000000};
     cppunivariate::CmpDomainResult result;
     static_cast<cppunivariate::CmpResult&>(result)=cppunivariate::cmp_mle(x,ctl,ledger);
     if(result.status==cppunivariate::CmpStatus::BudgetLimit) {
       result.status=cppunivariate::CmpStatus::NumericalFailure;
       result.stop_detail.budget_exhausted=false;
     }
     return result;
   }
   return cppunivariate::cmp_domain_mle(x,ctl);
 }();
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (res.status == cppunivariate::CmpStatus::Ok)
        return overridden(std::move(out), static_cast<int>(res.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(res.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
  std::string stop_kind;
  switch(res.status) {
    case cppunivariate::CmpStatus::GeometricLimit: stop_kind=outcome_kind(o);break;
    case cppunivariate::CmpStatus::TwoPointLimit: stop_kind=outcome_kind(o);break;
    case cppunivariate::CmpStatus::PointMassLimit: stop_kind=outcome_kind(o);break;
    default:break;
  }
  if(res.status==cppunivariate::CmpStatus::IterationFailure || res.status==cppunivariate::CmpStatus::NumericalFailure || res.status==cppunivariate::CmpStatus::BudgetLimit)stop_kind=outcome_kind(o);
  if(res.stop_detail.budget_exhausted)stop_kind=outcome_kind(o);
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,res.stop_detail,stop_kind,{"lambda","nu"});
  };

 switch(res.status) {
 case cppunivariate::CmpStatus::Ok: break;
 case cppunivariate::CmpStatus::GeometricLimit: return done(stopped_fail("The MLE approaches the geometric face; use mlgeom."));
 case cppunivariate::CmpStatus::TwoPointLimit: return done(stopped_fail("The MLE approaches an adjacent two-point law."));
 case cppunivariate::CmpStatus::PointMassLimit: return done(stopped_fail("The MLE approaches a point mass."));
 case cppunivariate::CmpStatus::InvalidInput: return done(stopped_fail("CMP requires nonnegative integer counts at most 2147483647, valid fixed parameters and positive controls."));
 default:
   if(res.stop_detail.budget_exhausted)return done(stopped_fail("CMP calculation exhausted its work budget. Raising iterlim may resolve this stop."));
   return done(stopped_fail("CMP calculation failed to converge or exceeded its work limit."));
 }
 std::vector<std::string> fixed;
 if(lambda.supplied) fixed.push_back("lambda");
 if(nu.supplied) fixed.push_back("nu");
 return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",lambda.supplied?ctl.fixed_lambda:std::exp(res.eta)},{"nu",res.nu}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
