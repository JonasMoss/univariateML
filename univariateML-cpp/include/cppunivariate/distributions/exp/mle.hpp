// Exponential MLE: rate = 1 / mean(x).

#pragma once
#include "../../core/precision.hpp"

#include "../../core/span.hpp"

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
  if (n == 0) return {kNaN, kNaN};

  double sum = 0.0;
  for (auto v : x) sum += v;
  const double dn = static_cast<double>(n);
  double rate = dn / sum;
  if (!std::isfinite(sum)) {
    CPPUNIV_EXTENDED_REAL total = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
    for (auto v : x) total += v;
    rate = static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(n) / total);
  }
  const double logLik = dn * (std::log(rate) - 1.0);
  return {rate, logLik};
}

inline ExpResult exp_mle(span x,double rate) {
  if(std::isnan(rate))return exp_mle(x);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(!(rate>0 && std::isfinite(rate)) || x.size()==0)return {nan,nan,ExpStatus::InvalidInput};
  CPPUNIV_EXTENDED_REAL total=0;for(double v:x){if(!(v>=0 && std::isfinite(v)))return {nan,nan,ExpStatus::InvalidInput};total+=v;}
  return {rate,static_cast<double>(x.size()*std::log(rate)-rate*total)};
}

}  // namespace cppunivariate
