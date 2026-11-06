#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/bhatt/mle.hpp"
namespace umvl_binding {
inline FitResult fit_bhatt(cppunivariate::span x, Held mu_arg = {}, Held sigma_arg = {}, Held a_arg = {}, double reltol=1e-8, int iterlim=500) {
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
  const auto fail = [&](const std::string& message) { return failure(message, warnings); };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, "nomle", {}); };
  const Held mu = mu_arg;
  const Held sigma = sigma_arg;
  const Held a = a_arg;

  const double f_sigma=sigma.supplied?sigma.value:cppunivariate::special::kNaN;
  const double f_a=a.supplied?a.value:cppunivariate::special::kNaN;
  if((sigma.supplied && !(std::isfinite(f_sigma) && f_sigma>0)) || (a.supplied && !(std::isfinite(f_a) && f_a>=0)))return fail("Fixed sigma must be positive and a nonnegative; both must be finite.");
  const double f_mu=mu.supplied?mu.value:cppunivariate::special::kNaN;
  if(mu.supplied&&!std::isfinite(f_mu))return fail("Held mu must be finite.");
  const auto f=cppunivariate::bhatt_mle(x,reltol,iterlim,f_sigma,f_a,f_mu);
  if(f.stop_detail.comparison_tie)return classify(fail("Bhatt fit failed: check inputs, reltol and iterlim."),f.stop_detail,"tie",{"mu","sigma","a"});
  if(f.status==cppunivariate::BhattStatus::Unbounded)return nomle("Bhattacharjee likelihood is unbounded for a constant sample.");
  if(f.status==cppunivariate::BhattStatus::NormalLimit) {
    if(sigma.supplied)return classify(fail("Bhattacharjee likelihood selects the normal face with sigma held. Use `mlnorm` for an unrestricted fit; it does not hold sigma."),f.stop_detail,"limit",{"mean","sd"},"norm");
    return classify(fail("Bhattacharjee likelihood selects the normal face: use `mlnorm`."),f.stop_detail,"limit",{"mean","sd"},"norm");
  }
  if(f.status==cppunivariate::BhattStatus::UniformLimit) {
    if(a.supplied)return classify(fail("Bhattacharjee likelihood selects the uniform face with a held. Use `mlunif` for an unrestricted fit; it does not hold a."),f.stop_detail,"limit",{"a","b"},"unif");
    return classify(fail("Bhattacharjee likelihood selects the uniform face: use `mlunif`."),f.stop_detail,"limit",{"a","b"},"unif");
  }
  if(f.status!=cppunivariate::BhattStatus::Ok)return fail("Bhatt fit failed: check inputs, reltol and iterlim.");
  std::vector<std::string> fixed;
  if(mu.supplied)fixed.push_back("mu");
  if(sigma.supplied)fixed.push_back("sigma");
  if(a.supplied)fixed.push_back("a");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mu",f.mu},{"sigma",f.sigma},{"a",f.a}}; out.logLik = f.logLik; out.iterations = f.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
