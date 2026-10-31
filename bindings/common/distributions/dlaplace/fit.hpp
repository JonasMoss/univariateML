#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/dlaplace/mle.hpp"
namespace umvl_binding {
inline FitResult fit_dlaplace(cppunivariate::ispan x, Held location_arg = {}, Held scale_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held location = location_arg;
  const Held scale = scale_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double mu=!location.supplied?nan:location.value, p=!scale.supplied?nan:scale.value;
  if((location.supplied&&std::isnan(mu))||(scale.supplied&&std::isnan(p)))return fail("Fixed parameters must not be missing.");
  auto r=cppunivariate::dlaplace_mle(x,mu,p);
  if(r.status==cppunivariate::DlaplaceStatus::PointMass) return fail("MLE escapes to a point mass (scale tends to zero).");
  if(r.status!=cppunivariate::DlaplaceStatus::Ok)return fail("Require integer location and scale strictly between zero and one.");
  if(r.not_unique)warnings.push_back(binding_message("Discrete Laplace location MLE is not unique; returning the smallest integer median."));
  std::vector<std::string> fixed;if(location.supplied)fixed.push_back("location");if(scale.supplied)fixed.push_back("scale");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"location",r.location},{"scale",r.scale}}; out.logLik = r.logLik; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
