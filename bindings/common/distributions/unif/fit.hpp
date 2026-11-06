#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/unif/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_unif(cppunivariate::span x, Held min_arg = {}, Held max_arg = {}) {
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
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return classify(failure(binding_message(format, arguments...), warnings), {}, "nomle", {}); };
  const Held min = min_arg;
  const Held max = max_arg;

  const double nan = std::numeric_limits<double>::quiet_NaN();

  const double checked_min = min.supplied ? min.value : nan;
  if (min.supplied && (!std::isfinite(checked_min) || (false && !(checked_min > 0)))) return fail("Held parameters must be finite and shapes/scales positive.");
  const double checked_max = max.supplied ? max.value : nan;
  if (max.supplied && (!std::isfinite(checked_max) || (false && !(checked_max > 0)))) return fail("Held parameters must be finite and shapes/scales positive.");

  std::vector<std::string> fixed;
  if (min.supplied) fixed.push_back("min");
  if (max.supplied) fixed.push_back("max");
  auto res = cppunivariate::unif_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, checked_min, checked_max);
  if (res.status == cppunivariate::UnifStatus::InvalidInput)
    return fail("Invalid held uniform endpoints: require finite min < max.");
  if (res.status == cppunivariate::UnifStatus::Incompatible)
    return fail("Data outside held uniform endpoints.");
  if (res.status == cppunivariate::UnifStatus::Unbounded) {
    return nomle("The uniform maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as max - min shrinks to zero.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"min",res.min},{"max",res.max}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; return out; }();

}
}
