// Log-logistic MLE: y = log(x), then Logistic MLE on y. Parameters are
// recovered analytically: shape = 1/scale_logistic, rate = exp(-mu_logistic).
// The log-likelihood is rederived in the log-logistic parameterisation.

#pragma once

#include "../../core/span.hpp"
#include "../logis/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class LogLogisticStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct LogLogisticResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
  LogLogisticStatus status = LogLogisticStatus::Ok;
};

inline LogLogisticResult loglogistic_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  if (std::all_of(x.begin(),x.end(),[&](double v){return v==x[0] && v>0 && std::isfinite(v);}))
    return {kNaN,kNaN,kNaN,0,LogLogisticStatus::Unbounded};

  std::vector<double> y(n);
  for (std::size_t i = 0; i < n; ++i) y[i] = std::log(x[i]);
  LogisticResult base = logistic_mle(span{y.data(), n}, tol, maxit);

  const double shape = 1.0 / base.scale;
  const double rate = std::exp(-base.mu);
  // Change of variables avoids powers of extreme observations.
  double sum_log_x = 0.0;
  for (auto v : y) sum_log_x += v;
  const double logLik = base.logLik - sum_log_x;
  return {shape, rate, logLik, base.iterations,
      base.status == LogisticStatus::Ok ? LogLogisticStatus::Ok :
      LogLogisticStatus::IterationFailure};
}

inline LogLogisticResult loglogistic_mle(span x,double tol,int maxit,double shape,double rate) {
  if(std::isnan(shape) && std::isnan(rate))return loglogistic_mle(x,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0 || !(tol>0 && std::isfinite(tol)) || maxit<=0 || (!std::isnan(shape) && !(shape>0 && std::isfinite(shape))) ||
     (!std::isnan(rate) && !(rate>0 && std::isfinite(rate))))return {nan,nan,nan,0,LogLogisticStatus::InvalidInput};
  std::vector<double> y;
  for(double v:x){if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,0,LogLogisticStatus::InvalidInput};y.push_back(std::log(v));}
  const double n=static_cast<double>(x.size());int iterations=0;
  if(std::isnan(rate)) {
    double lo=y[0],hi=y[0];for(double v:y){lo=std::min(lo,v);hi=std::max(hi,v);}
    for(int j=0;j<maxit && hi-lo>1e-12;++j){double mid=(lo+hi)/2,s=0;for(double v:y)s+=std::tanh(shape*(v-mid)/2);if(s>0)lo=mid;else hi=mid;++iterations;}
    if(hi-lo>1e-12)return {shape,nan,nan,iterations,LogLogisticStatus::IterationFailure};
    rate=std::exp(-(lo+hi)/2);
  }
  const double loc=-std::log(rate);
  if(std::isnan(shape)) {
    bool equal=true;for(double v:x)equal=equal && v==1/rate;
    if(equal)return {nan,rate,nan,0,LogLogisticStatus::Unbounded};
    double lo=-745,hi=710;
    for(int j=0;j<maxit && hi-lo>1e-12;++j){double mid=(lo+hi)/2,k=std::exp(mid);long double s=n/k;for(double v:y){double d=v-loc;s-=d*std::tanh(k*d/2);}if(s>0)lo=mid;else hi=mid;++iterations;}
    if(hi-lo>1e-12)return {nan,rate,nan,iterations,LogLogisticStatus::IterationFailure};
    shape=std::exp((lo+hi)/2);
  }
  double ll=0;for(double v:y){const double z=shape*(v-loc);ll+=std::log(shape)-v-std::abs(z)-2*std::log1p(std::exp(-std::abs(z)));}
  return {shape,rate,ll,iterations};
}

}  // namespace cppunivariate
