#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/sgomp/mle.hpp"
namespace umvl_binding {
inline FitResult fit_sgomp(cppunivariate::span x, Held b_arg = {}, Held eta_arg = {}, double reltol=1e-10, int iterlim=200) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(reltol>0 && std::isfinite(reltol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v) && v>=0;}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, "nomle", {}); };
  const Held b = b_arg;
  const Held eta = eta_arg;

  const double bv=b.supplied ? b.value : cppunivariate::sgomp_detail::nan();
  const double ev=eta.supplied ? eta.value : cppunivariate::sgomp_detail::nan();
  if (b.supplied && !(bv>0 && std::isfinite(bv))) return fail("fixed `b` must be positive and finite.");
  if (eta.supplied && !(ev>=0 && std::isfinite(ev))) return fail("fixed `eta` must be nonnegative and finite.");
  const auto f=cppunivariate::sgomp_mle(x,reltol,iterlim,bv,ev);
  if (f.status==cppunivariate::SgompStatus::Collapse) return nomle("The shifted Gompertz MLE does not exist: constant data have unbounded likelihood.");
  if (f.status==cppunivariate::SgompStatus::InvalidInput) return fail("Shifted Gompertz requires finite nonnegative data, positive reltol and positive iterlim.");
  if (f.status!=cppunivariate::SgompStatus::Ok) {
    if(f.stop_detail.budget_exhausted)return classify(fail("Shifted Gompertz certificate budget exhausted; increase iterlim."),f.stop_detail,"budget",{"b","eta"});
    if(f.stop_detail.comparison_tie)return classify(fail("Shifted Gompertz competing global maxima cannot be distinguished; increase iterlim."),f.stop_detail,"tie",{"b","eta"});
    return classify(fail("Shifted Gompertz MLE failed to converge; increase iterlim."),f.stop_detail,"numerical",{"b","eta"});
  }
  std::vector<std::string> fixed;
  if (b.supplied) fixed.push_back("b");
  if (eta.supplied) fixed.push_back("eta");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"b",f.b},{"eta",f.eta}}; out.logLik = f.logLik; out.iterations = f.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
