#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/norm/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_norm(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}) {
  const auto classify = [&](FitResult out, const cppunivariate::StopDetail& detail,
      const std::string& kind, const std::vector<std::string>& names,
      const std::string& model="") {
    if(x.empty() || !std::all_of(x.begin(),x.end(),[](double v){return std::isfinite(v);}))return out;
    if(kind=="nomle" && !std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))return out;
    if(!finite_stop_payload(detail,kind))return out;
    return stop_failure(out,detail,kind,names,model);
  };
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held sd = sd_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_mean=mean.supplied?mean.value:nan;
  if(mean.supplied && (!std::isfinite(held_mean)))return invalid_argument(fail("`mean` must be finite."));
  const double held_sd=sd.supplied?sd.value:nan;
  if(sd.supplied && (!std::isfinite(held_sd) || !(held_sd>0)))return invalid_argument(fail("`sd` must be positive and finite."));
  std::vector<std::string> fixed;
  if(mean.supplied)fixed.push_back("mean");
  if(sd.supplied)fixed.push_back("sd");

  auto res =
      cppunivariate::norm_mle(x,held_mean,held_sd);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) { return with_outcome(std::move(out), o, static_cast<int>(res.status)); };
  if (res.status == cppunivariate::NormStatus::Unbounded) {
    return done(classify(fail("The norm maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as scale shrinks to zero."), {}, outcome_kind(o), {}));
  }
  if (res.status == cppunivariate::NormStatus::InvalidInput) return done(fail("Norm requires nonempty finite data and valid held parameters."));
  if (res.status != cppunivariate::NormStatus::Ok) return done(fail("Norm likelihood exceeds double precision."));
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",res.mean},{"sd",res.sd}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; return done(out); }();

}
}
