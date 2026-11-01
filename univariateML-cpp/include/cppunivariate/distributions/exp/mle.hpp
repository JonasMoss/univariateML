// Exponential MLE: rate = 1 / mean(x).

#pragma once
#include "../../core/precision.hpp"

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class ExpStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct ExpResult {
  double rate;
  double logLik;
  ExpStatus status = ExpStatus::Ok;
};

inline ExpResult exp_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, ExpStatus::InvalidInput};

  double sum = 0.0;
  for (auto v : x) {
    if (!(v >= 0 && std::isfinite(v))) return {kNaN,kNaN,ExpStatus::InvalidInput};
    sum += v;
  }
  const double dn = static_cast<double>(n);
  double rate = dn / sum;
  if (!std::isfinite(sum)) {
    // Scale before summation: long double has double range on some targets.
    double largest = 0.0;
    for (double v : x) largest = std::max(largest, v);
    double scaled = 0.0;
    for (double v : x) scaled += v / largest;
    rate = (dn / scaled) / largest;
  }
  const double logLik = dn * (std::log(rate) - 1.0);
  return {rate, logLik, sum == 0.0 ? ExpStatus::Unbounded :
    (!(rate > 0) || !std::isfinite(rate) || !std::isfinite(logLik)) ? ExpStatus::IterationFailure : ExpStatus::Ok};
}

inline ExpResult exp_mle(span x,double rate) {
  if(std::isnan(rate))return exp_mle(x);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(!(rate>0 && std::isfinite(rate)) || x.size()==0)return {nan,nan,ExpStatus::InvalidInput};
  CPPUNIV_EXTENDED_REAL total=0;for(double v:x){if(!(v>=0 && std::isfinite(v)))return {nan,nan,ExpStatus::InvalidInput};total+=v;}
  if (!std::isfinite(total)) {
    double weighted = 0.0;
    for (double v : x) weighted += rate * v;
    return {rate, static_cast<double>(x.size()) * std::log(rate) - weighted};
  }
  return {rate,static_cast<double>(x.size()*std::log(rate)-rate*total)};
}

}  // namespace cppunivariate
