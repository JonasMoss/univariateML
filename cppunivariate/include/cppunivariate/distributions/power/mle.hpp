// Power distribution MLE: closed form using log(x) statistics.
//
// alpha = max(x) + epsilon, beta = 1 / (log(alpha) - mean(log x)).
// Returns NaN params if min(x) <= 0; the caller turns that into an error.

#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class PowerStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct PowerResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
  PowerStatus status = PowerStatus::Ok;
};

inline PowerResult power_mle(span x, double epsilon) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  double xmin = x[0];
  double xmax = x[0];
  for (std::size_t i = 1; i < n; ++i) {
    xmin = std::min(xmin, x[i]);
    xmax = std::max(xmax, x[i]);
  }
  if (xmin <= 0.0) return {kNaN, kNaN, kNaN, 0};

  double sum_log = 0.0;
  for (auto v : x) sum_log += std::log(v);
  const double m = sum_log / static_cast<double>(n);
  double alpha = xmax * (1.0 + epsilon);
  if (epsilon>0.0 && alpha == xmax) alpha = std::nextafter(xmax,std::numeric_limits<double>::infinity());
  long double gap=0.0L;
  for(auto v:x) gap+=std::log1p((static_cast<long double>(alpha)-v)/v);
  const double beta=static_cast<double>(static_cast<long double>(n)/gap);
  const double logLik = static_cast<double>(n) *
      (std::log(beta) - m - 1.0);
  return {alpha, beta, logLik, 0};
}

// Free endpoint closure belongs to P3. A held endpoint must strictly
// exceed the data under the present reference convention.
inline PowerResult power_mle(span x,double epsilon,double alpha,double beta) {
  if(std::isnan(alpha) && std::isnan(beta))return power_mle(x,epsilon);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0 || !(alpha>0 && std::isfinite(alpha)) ||
     (!std::isnan(beta) && !(beta>0 && std::isfinite(beta))))return {nan,nan,nan,0,PowerStatus::InvalidInput};
  long double gap=0,logs=0;for(double v:x){if(!(v>0 && v<alpha && std::isfinite(v)))return {nan,nan,nan,0,PowerStatus::InvalidInput};gap+=std::log1p((static_cast<long double>(alpha)-v)/v);logs+=std::log(v);}
  const double n=static_cast<double>(x.size());if(std::isnan(beta))beta=static_cast<double>(n/gap);
  return {alpha,beta,static_cast<double>(n*std::log(beta)-beta*gap-logs),0};
}

}  // namespace cppunivariate
