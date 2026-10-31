// Pareto MLE: closed form. xmin = min(x); shape = n / sum(log(x/xmin)).

#pragma once
#include "../../core/precision.hpp"

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class ParetoStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct ParetoResult {
  double shape;
  double min;
  double logLik;
  ParetoStatus status = ParetoStatus::Ok;
};

inline ParetoResult pareto_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN, ParetoStatus::InvalidInput};

  for (double v : x) if (!(v > 0 && std::isfinite(v)))
    return {kNaN,kNaN,kNaN,ParetoStatus::InvalidInput};

  double xmin = x[0];
  for (std::size_t i = 1; i < n; ++i) xmin = std::min(xmin, x[i]);
  if (xmin <= 0.0) return {kNaN, kNaN, kNaN, ParetoStatus::InvalidInput};

  double sum_log_ratio = 0.0;
  double sum_log_x = 0.0;
  for (auto v : x) {
    sum_log_ratio += static_cast<double>(std::log1p((static_cast<CPPUNIV_EXTENDED_REAL>(v)-xmin)/xmin));
    sum_log_x += std::log(v);
  }
  const double dn = static_cast<double>(n);
  const double alpha = dn / sum_log_ratio;
  const double logLik = dn * (std::log(alpha)-1.0) - sum_log_x;
  return {alpha, xmin, logLik, sum_log_ratio == 0.0 ? ParetoStatus::Unbounded : std::isfinite(alpha) && std::isfinite(logLik) ? ParetoStatus::Ok : ParetoStatus::IterationFailure};
}

inline ParetoResult pareto_mle(span x,double a,double b) {
  if(std::isnan(a) && std::isnan(b))return pareto_mle(x);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0 || (!std::isnan(a) && !(a>0 && std::isfinite(a))) ||
     (!std::isnan(b) && !(b>0 && std::isfinite(b))))return {nan,nan,nan,ParetoStatus::InvalidInput};
  double low=x[0];for(double v:x){if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,ParetoStatus::InvalidInput};low=std::min(low,v);}
  if(std::isnan(b))b=low;
  if(b>low)return {nan,b,nan,ParetoStatus::InvalidInput};
  CPPUNIV_EXTENDED_REAL gap=0,logs=0;for(double v:x){gap+=std::log1p((static_cast<CPPUNIV_EXTENDED_REAL>(v)-b)/b);logs+=std::log(v);}
  const double n=static_cast<double>(x.size());
  if(std::isnan(a) && gap==0)return {nan,b,nan,ParetoStatus::Unbounded};
  if(std::isnan(a))a=static_cast<double>(n/gap);
  const double ll=static_cast<double>(n*std::log(a)-a*gap-logs);
  return {a,b,ll,std::isfinite(a) && std::isfinite(ll) ? ParetoStatus::Ok : ParetoStatus::IterationFailure};
}

}  // namespace cppunivariate
