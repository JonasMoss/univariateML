#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
namespace umvl_binding {
inline FitResult fit_frechet(cppunivariate::span x, Held lambda_arg = {}, Held mu_arg = {}, Held sigma_arg = {}, double reltol=1e-10, int iterlim=100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held lambda = lambda_arg;
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;

 if(mu.supplied && !std::isfinite(mu.value)) return invalid_argument(fail("mu must be finite."));
 const double shape=lambda.supplied ? lambda.value : std::numeric_limits<double>::quiet_NaN();
 const double scale=sigma.supplied ? sigma.value : std::numeric_limits<double>::quiet_NaN();
 if(lambda.supplied && (!(shape>0) || !std::isfinite(shape))) return invalid_argument(fail("lambda must be positive and finite."));
 if(sigma.supplied && (!(scale>0) || !std::isfinite(scale))) return invalid_argument(fail("sigma must be positive and finite."));
 std::vector<std::string> fixed;
 if(lambda.supplied) fixed.push_back("lambda");
 if(mu.supplied) fixed.push_back("mu");
 if(sigma.supplied) fixed.push_back("sigma");
 const auto r=cppunivariate::frechet_mle(x,reltol,iterlim,
 mu.supplied ? mu.value : std::numeric_limits<double>::quiet_NaN(),shape,scale);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    // This classification uses observations/holds absent from the core result.
    if (out.error_kind == "nomle" && o.reason == cppunivariate::OutcomeReason::InvalidInput)
      return with_outcome(std::move(out), cppunivariate::FitOutcome::nonexistence(cppunivariate::OutcomeReason::Unbounded), static_cast<int>(r.status));
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(r.status));
    return with_outcome(std::move(out), o, static_cast<int>(r.status));
  };

 const auto certificate_stop=[&](FitResult out,const std::string& kind) {
   auto result=stop_failure(out,r.stop_detail,kind,{"lambda","mu","sigma"});
   if(r.stop_detail.limit_coef.size()==2) {
     result.model="gumbel";
     result.limit_estimates={{"mu",r.stop_detail.limit_coef[0]},{"sigma",r.stop_detail.limit_coef[1]}};
   }
   return result;
 };
 if(r.status==cppunivariate::FrechetStatus::SearchContradicted)
   return done(certificate_stop(fail("Frechet search is disproved by a certified better feasible point."),outcome_kind(o)));
 if(r.stop_detail.budget_exhausted)
   return done(certificate_stop(fail("Frechet certificate exhausted its work budget; raise iterlim."),outcome_kind(o)));
 if(r.stop_detail.comparison_tie)
   return done(certificate_stop(fail("Frechet certificate cannot separate competing finite maxima or the Gumbel face within its floating-point margin."),"tie"));
 if(r.status==cppunivariate::FrechetStatus::NumericalFailure ||
    (r.status==cppunivariate::FrechetStatus::IterationFailure && !std::isnan(r.stop_detail.loglik_upper)))
   return done(certificate_stop(fail("Frechet certificate failed to converge or resolve the maximum numerically; raise iterlim for an exhausted solve."),outcome_kind(o)));
 if(r.status==cppunivariate::FrechetStatus::GumbelLimit) {
   auto out=fail("Frechet selects the Gumbel limit. Use `mlgumbel`.");
   if(!r.stop_detail.limit_coef.empty())return done(stop_failure(out,r.stop_detail,outcome_kind(o),{"mu","sigma"},"gumbel"));
   return done(out);
 }
 if(r.status==cppunivariate::FrechetStatus::NoMLE || (r.status==cppunivariate::FrechetStatus::InvalidData && !std::isnan(r.stop_detail.loglik_upper))) return done(stop_failure(fail("Frechet has no finite maximum: minimum multiplicity permits scale collapse."),r.stop_detail,outcome_kind(o),{"lambda","mu","sigma"}));
 if(r.status==cppunivariate::FrechetStatus::InvalidData && !mu.supplied&&!lambda.supplied&&!sigma.supplied &&
     x.size()>=3 && reltol>0&&std::isfinite(reltol)&&iterlim>0 &&
     std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);})) {
   const double minimum=*std::min_element(x.begin(),x.end());
   if(2*static_cast<std::size_t>(std::count(x.begin(),x.end(),minimum))>x.size()) {
     cppunivariate::StopDetail detail;detail.loglik_upper=INFINITY;
     return done(stop_failure(fail("Frechet has no finite maximum: a tied minimum majority permits scale collapse; free shape and scale require nonconstant data."),detail,"nomle",{"lambda","mu","sigma"}));
   }
 }
 if(r.status==cppunivariate::FrechetStatus::InvalidData) return done(fail("Frechet requires finite observations strictly above held mu and compatible holds. Free shape and scale require nonconstant data; the default free-mu fit requires at least three observations and admissible minimum tie multiplicity. Constant data can have an unbounded shape likelihood and fixed shape can have an unattained collapse face. Tolerance and iteration limit must be positive."));
 if(r.status!=cppunivariate::FrechetStatus::Ok) return done(fail("Frechet profile search failed to converge."));
  std::vector<std::pair<std::string,std::string>> restricted;
  if(r.restricted & 1u) restricted.emplace_back("lambda","lower");
  if(r.restricted & 2u) restricted.emplace_back("lambda","upper");
  if(r.restricted & 4u) restricted.emplace_back("mu","lower");
  if(r.restricted & 8u) restricted.emplace_back("mu","upper");
  if(r.restricted & 16u) restricted.emplace_back("sigma","lower");
  if(r.restricted & 32u) restricted.emplace_back("sigma","upper");
 return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",r.lambda},{"mu",r.mu},{"sigma",r.sigma}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; if(!lambda.supplied && !mu.supplied)out.extra.emplace_back("certified",r.certified?1.:0.); else if(!r.certified)out.extra.push_back({"certified",0}); return out; }());

}
}
