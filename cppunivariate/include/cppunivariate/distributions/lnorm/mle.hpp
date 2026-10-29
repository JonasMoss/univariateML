// Lognormal MLE: y = log(x), then Normal MLE on y. Adjusted log-likelihood
// includes the change-of-variable Jacobian -sum(log(x)).

#pragma once

#include "../../core/span.hpp"
#include "../norm/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class LognormalStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct LognormalResult {
  double meanlog;
  double sdlog;
  double logLik;
  LognormalStatus status = LognormalStatus::Ok;
};

inline LognormalResult lognormal_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN};

  std::vector<double> y(n);
  double sum_log = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    y[i] = std::log(x[i]);
    sum_log += y[i];
  }
  const auto r = norm_mle(span{y.data(), n});
  return {r.mean, r.sd, r.logLik - sum_log};
}

inline LognormalResult lognormal_mle(span x,double meanlog,double sdlog) {
  if(std::isnan(meanlog) && std::isnan(sdlog))return lognormal_mle(x);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0 || (!std::isnan(meanlog) && !std::isfinite(meanlog)) ||
     (!std::isnan(sdlog) && !(sdlog>0 && std::isfinite(sdlog))))return {nan,nan,nan,LognormalStatus::InvalidInput};
  std::vector<double> y;double jac=0;
  for(double v:x){if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,LognormalStatus::InvalidInput}; y.push_back(std::log(v));jac-=std::log(v);}
  const double n=static_cast<double>(x.size());
  if(std::isnan(meanlog))meanlog=norm_detail::moments(span{y.data(),y.size()}).mean;
  long double ss=0;for(double v:y){const long double d=static_cast<long double>(v)-meanlog;ss+=d*d;}
  if(std::isnan(sdlog))sdlog=static_cast<double>(std::sqrt(ss/n));
  if(!(sdlog>0))return {meanlog,sdlog,nan,LognormalStatus::Unbounded};
  const double ll=static_cast<double>(jac-n*(std::log(sdlog)+0.5*std::log(2*M_PI))-ss/(2*static_cast<long double>(sdlog)*sdlog));
  return {meanlog,sdlog,ll};
}

}  // namespace cppunivariate
