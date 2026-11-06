#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/beta/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_beta(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, double reltol = 1.490116e-08, int iterlim = 100) {
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
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;

  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double checked_shape1 = shape1.supplied ? shape1.value : nan;
  if (shape1.supplied && (!std::isfinite(checked_shape1) || (true && !(checked_shape1 > 0)))) return failure("Held parameters must be finite and shapes/scales positive.");
  const double checked_shape2 = shape2.supplied ? shape2.value : nan;
  if (shape2.supplied && (!std::isfinite(checked_shape2) || (true && !(checked_shape2 > 0)))) return failure("Held parameters must be finite and shapes/scales positive.");


  std::vector<std::string> fixed;
  if (shape1.supplied) fixed.push_back("shape1");
  if (shape2.supplied) fixed.push_back("shape2");
  auto res = cppunivariate::beta_mle(x, reltol, iterlim, checked_shape1, checked_shape2);
  if (res.status == cppunivariate::BetaStatus::InvalidInput) return failure("Beta requires nonempty finite data strictly between zero and one and valid held parameters.");
  if (res.status == cppunivariate::BetaStatus::Degenerate) {
    return classify(failure("The beta maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as both shapes grow."),{},"nomle",{},"");
  }
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    return failure("Beta MLE failed: iteration limit or shapes cannot be resolved in double precision; increase iterlim.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.alpha},{"shape2",res.beta}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }();

}
}
