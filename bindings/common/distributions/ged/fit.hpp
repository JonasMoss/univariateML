#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/ged/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_ged(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}, Held nu_arg = {}, double gradtol = 1.220703e-4, int iterlim = 200) {
  // Invalid controls remain ordinary errors, even if a legacy fitter reports
  // a statistical status before checking them.
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(!(gradtol>0 && std::isfinite(gradtol) && iterlim>0))return out;
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held sd = sd_arg;
  const Held nu = nu_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<std::string> fixed;
  const double held_mean=mean.supplied?mean.value:nan;
  if(mean.supplied && (!std::isfinite(held_mean)))return fail("Invalid held `mean`.");
  if(mean.supplied)fixed.push_back("mean");
  const double held_sd=sd.supplied?sd.value:nan;
  if(sd.supplied && (!std::isfinite(held_sd)||!(held_sd>0)))return fail("Invalid held `sd`.");
  if(sd.supplied)fixed.push_back("sd");
  const double held_nu=nu.supplied?nu.value:nan;
  if(nu.supplied && (!std::isfinite(held_nu)||!(held_nu>0)))return fail("Invalid held `nu`.");
  if(nu.supplied)fixed.push_back("nu");
  auto res = cppunivariate::ged_mle(x, gradtol, iterlim, held_mean, held_sd, held_nu);
  if(res.status==cppunivariate::GedStatus::InvalidShape)return fail("Held `nu` must be at least 1 under the existing GED restriction.");
  if(res.sigma==0 && (mean.supplied||sd.supplied||nu.supplied))
    return classify(fail("GED scale likelihood is unbounded: all observations equal the fitted or held location."),{},"nomle",{});
  if(res.stop_detail.budget_exhausted)return classify(fail("GED shape certificate did not converge within its work budget; increase iterlim."),res.stop_detail,"budget",{"mean","sd","nu"});
  if(res.status==cppunivariate::GedStatus::UniformUnresolved)return classify(fail("GED held-sd uniform comparison is unresolved by the shape certificate."),res.stop_detail,"numerical",{"mean","sd","nu"});
  if(res.status==cppunivariate::GedStatus::Nonunique)return classify(fail("GED maxima are not unique or their comparison is unresolved, including a possible Laplace median interval."),res.stop_detail,"tie",{"mean","sd","nu"});
  if(res.stop_detail.comparison_tie)return classify(fail("Conditional GED search did not converge; increase iterlim."),res.stop_detail,"tie",{"mean","sd","nu"});
  if (res.status == cppunivariate::GedStatus::InvalidStart) {
    if(!sd.supplied && gradtol>0 && std::isfinite(gradtol) && iterlim>0 && !x.empty() && std::isfinite(x[0]) && (!mean.supplied || held_mean==x[0]) && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return classify(fail("GED MLE requires at least 2 finite observations with positive "
               "variance."),{},"nomle",{});
    return fail("GED MLE requires at least 2 finite observations with positive "
               "variance.");
  }
  if (res.status == cppunivariate::GedStatus::UniformBoundary) {
    return classify(fail("GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead."),res.stop_detail,"limit",{"a","b"},"unif");
  }
  if(res.status==cppunivariate::GedStatus::TransformFailure)return classify(fail("GED normalization loses coefficient or likelihood accuracy; no finite fit is certified."),res.stop_detail,"numerical",{"mean","sd","nu"});
  if(res.status==cppunivariate::GedStatus::BfgsFailure)return classify(fail("Conditional GED search did not converge; increase iterlim."),res.stop_detail,"numerical",{"mean","sd","nu"});
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu)) {
    return fail("GED MLE failed to converge (check inputs)");
  }
  std::vector<std::pair<std::string,std::string>> restricted;
  if(res.restricted & 1u) restricted.emplace_back("mean","lower");
  if(res.restricted & 2u) restricted.emplace_back("mean","upper");
  if(res.restricted & 4u) restricted.emplace_back("sd","lower");
  if(res.restricted & 8u) restricted.emplace_back("sd","upper");
  if(res.restricted & 16u) restricted.emplace_back("nu","lower");
  if(res.restricted & 32u) restricted.emplace_back("nu","upper");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",res.mu},{"sd",res.sigma},{"nu",res.nu}}; out.restricted = restricted; out.has_restricted = true; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }();

}
}
