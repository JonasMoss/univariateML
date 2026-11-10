#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/sn/mle.hpp"
namespace umvl_binding {
inline FitResult fit_sn(cppunivariate::span x, Held xi_arg = {}, Held omega_arg = {}, Held alpha_arg = {}, double reltol=1e-10, int iterlim=100) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(reltol>0 && std::isfinite(reltol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held xi = xi_arg;
  const Held omega = omega_arg;
  const Held alpha = alpha_arg;

  const double nan=cppunivariate::special::kNaN;
  const double c=xi.supplied ? xi.value : nan;
  const double w=omega.supplied ? omega.value : nan;
  const double a=alpha.supplied ? alpha.value : nan;
  if ((xi.supplied && !std::isfinite(c)) ||
      (omega.supplied && !(std::isfinite(w) && w>0)) ||
      (alpha.supplied && !std::isfinite(a))) return fail("Fixed xi and alpha must be finite; fixed omega must be finite and positive.");
  const auto fit=cppunivariate::sn_mle(x,reltol,iterlim,c,w,a);
  if(fit.stop_detail.comparison_tie && std::isfinite(fit.stop_detail.loglik_upper) &&
     fit.stop_detail.loglik_lower==fit.stop_detail.loglik_upper)
    return classify(fail("Skew-normal shape is not identifiable when every observation equals the held location."),fit.stop_detail,"tie",{"xi","omega","alpha"});
  if(fit.stop_detail.comparison_tie)return classify(fail("Skew-normal search did not converge or could not certify the maximum; increase iterlim."),fit.stop_detail,"tie",{"xi","omega","alpha"});
  if(fit.status==cppunivariate::SnStatus::AccuracyFailure)return fail("sn numerical accuracy failure: coefficients cannot be represented at the requested accuracy.");
  if(fit.status==cppunivariate::SnStatus::RightHalfNormalBoundary)
    return classify(fail("The skew-normal MLE is infinite: use `mlhnorm`."),fit.stop_detail,"limit",{"mu","sigma"},"hnorm");
  if(fit.status==cppunivariate::SnStatus::LeftHalfNormalBoundary)
    return classify(fail("The skew-normal MLE is infinite: use `mlhnorm` on `-x`."),fit.stop_detail,"limit",{"mu","sigma"},"unavailable:reflected-half-normal");
  if(fit.status==cppunivariate::SnStatus::InvalidInput) return (x.size()>=2 && !omega.supplied && (!xi.supplied || c==x[0]) && std::isfinite(x[0]) && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) ? classify(fail("Skew-normal fitting requires distinct finite observations, positive reltol and positive iterlim."),{},"nomle",{}) : fail("Skew-normal fitting requires distinct finite observations, positive reltol and positive iterlim.");
  if(fit.budget_exhausted)return classify(fail("Skew-normal search did not converge or could not certify the maximum; increase iterlim."),fit.stop_detail,"budget",{"xi","omega","alpha"});
  if(fit.status!=cppunivariate::SnStatus::Ok) return classify(fail("Skew-normal search did not converge or could not certify the maximum; increase iterlim."),fit.stop_detail,"numerical",{"xi","omega","alpha"});
  std::vector<std::string> fixed;
  if(xi.supplied) fixed.push_back("xi");
  if(omega.supplied) fixed.push_back("omega");
  if(alpha.supplied) fixed.push_back("alpha");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"xi",fit.xi},{"omega",fit.omega},{"alpha",fit.alpha}}; out.logLik = fit.logLik; out.iterations = fit.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
