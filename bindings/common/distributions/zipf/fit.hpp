#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/zipf/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_zipf(cppunivariate::ispan x, Held N_arg = {}, Held s_arg = {}, double reltol = 1.220703e-4, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held N = N_arg;
  const Held s = s_arg;

  if (x.size() == 0) {
    return fail("x must have positive length");
  }
  if (x.size() > 0 &&
      std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    warnings.push_back(binding_message("all observations are zero"));
  }

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double hn=!N.supplied?nan:N.value, hs=!s.supplied?nan:s.value;
  if ((N.supplied&&!std::isfinite(hn)) || (s.supplied&&!std::isfinite(hs)))
    return fail("Held coefficients must be finite.");
  auto res = cppunivariate::zipf_mle(x, reltol, iterlim,hn,hs);
  if (res.status==cppunivariate::ZipfStatus::PointMass)
    return fail("The MLE does not exist: point mass at one with held N > 1.");
  if (res.status==cppunivariate::ZipfStatus::InvalidInput)
    return fail("Zipf requires positive integer observations, integer N >= max(x) at most INT_MAX, and finite s >= 0.");
  if (res.status == cppunivariate::ZipfStatus::AllOnes) {
    warnings.push_back(binding_message("All observations are equal to 1. The maximum likelihood "
                  "estimator is not unique."));
  }
  if (res.status == cppunivariate::ZipfStatus::ShapeNegative) {
    return fail("Optimal shape parameter is less than 0. The maximum likelihood "
               "estimator does not exist.");
  }
  if (res.status != cppunivariate::ZipfStatus::Ok &&
      res.status != cppunivariate::ZipfStatus::AllOnes) {
    return fail("Zipf MLE failed to converge (check data)");
  }
  if (!std::isfinite(res.N) || !std::isfinite(res.shape)) {
    return fail("Zipf MLE failed to converge (check data)");
  }
  std::vector<std::string> fixed;
  if(N.supplied)fixed.push_back("N");
  if(s.supplied)fixed.push_back("s");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"N",res.N},{"s",res.shape}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
