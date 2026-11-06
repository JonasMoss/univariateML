#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gompertz/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_gompertz(cppunivariate::span x, Held a_arg = {}, Held b_arg = {}, double reltol = 1.220703e-4, int iterlim = 100) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(reltol>0 && std::isfinite(reltol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v) && v>=0;}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, "nomle", {}); };
  const Held a = a_arg;
  const Held b = b_arg;

  std::vector<std::string> fixed;
  const double a_held = !a.supplied ? std::numeric_limits<double>::quiet_NaN() : a.value;
  if (a.supplied) {
    if (!(std::isfinite(a_held) && a_held > 0.0)) return fail("Held `a` must be positive and finite.");
    fixed.push_back("a");
  }
  const double b_held = !b.supplied ? std::numeric_limits<double>::quiet_NaN() : b.value;
  if (b.supplied) {
    if (!(std::isfinite(b_held) && b_held > 0.0)) return fail("Held `b` must be positive and finite.");
    fixed.push_back("b");
  }

  auto res = cppunivariate::gompertz_mle(x, reltol, iterlim, a_held, b_held);
  if(res.status==cppunivariate::GompertzStatus::Unbounded)
    return nomle("The Gompertz maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");

  if (res.status == cppunivariate::GompertzStatus::Overdispersed) {
    return classify(fail("The Gompertz maximum likelihood estimator escapes to an exponential "
               "when the score at b=0 is nonpositive. Use `mlexp` instead."),res.stop_detail,"limit",{"rate"},"exp");
  }
  if (res.status != cppunivariate::GompertzStatus::Ok || !std::isfinite(res.a) ||
      !std::isfinite(res.b)) {
    if (!a.supplied && std::all_of(x.begin(),x.end(),[](double v){return v==0;}))
      return fail("The Gompertz likelihood is unbounded for all-zero data with a free.");
    return fail("Gompertz MLE failed to converge or data exceed double precision.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"a",res.a},{"b",res.b}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
