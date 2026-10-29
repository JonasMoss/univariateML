// Actuar rate is the reciprocal of the exponential rate on 1/x.
#pragma once
#include "../exp/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
enum class InvExpStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct InvExpResult { double rate; double logLik; int iterations;    InvExpStatus status = InvExpStatus::Ok;
};
inline InvExpResult invexp_mle(span x) {
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v) || !std::isfinite(1/v)) {
      const double nan=std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,0};
    }
  }
  std::vector<double> y(x.size());
  double jacobian = 0;
  for (std::size_t i=0; i<x.size(); ++i) { y[i]=1.0/x[i]; jacobian -= 2*std::log(x[i]); }
  auto r = exp_mle(span{y.data(), y.size()});
  return {1.0/r.rate, r.logLik+jacobian, 0};
}

inline InvExpResult invexp_mle(span x,double rate) {
  if(std::isnan(rate))return invexp_mle(x);
  std::vector<double> y;double jac=0;for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
  const auto r=exp_mle(span{y.data(),y.size()},1/rate);
  return {rate,r.logLik+jac,0,r.status==ExpStatus::Ok ? InvExpStatus::Ok : InvExpStatus::InvalidInput};
}

} // namespace cppunivariate
