// Zero-Inflated Poisson MLE. Closed-form via Lambert W: when the empirical
// zero-rate exceeds exp(-mean), the principal branch W_0 yields the MLE.

#pragma once

#include "../../core/lambert_w.hpp"
#include "../../core/span.hpp"
#include "../pois/mle.hpp"
#include "../ztpois/profile.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class ZipStatus { Ok = 0, InvalidInput, NumericalFailure, ZeroLimit };

struct ZipResult {
  double lambda;
  double pi;
  double logLik;
  ZipStatus status;
};


inline ZipResult zip_mle(ispan x,
    double fixed_lambda=std::numeric_limits<double>::quiet_NaN(),
    double fixed_pi=std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {kNaN, kNaN, kNaN, ZipStatus::InvalidInput};

  double sum_zero = 0.0, sum_x = 0.0;
  for (auto v : x) {
    if (v < 0) return {kNaN, kNaN, kNaN, ZipStatus::InvalidInput};
    if (v == 0) sum_zero += 1.0;
    sum_x += static_cast<double>(v);
  }
  const double dn = static_cast<double>(n);
  const double r0 = sum_zero / dn;
  const double x_bar = sum_x / dn;

  const bool fl=!std::isnan(fixed_lambda), fp=!std::isnan(fixed_pi);
  if ((fl && (!(fixed_lambda>0)||!std::isfinite(fixed_lambda))) ||
      (fp && (!(fixed_pi>=0 && fixed_pi<=1)||!std::isfinite(fixed_pi))) ||
      (fp && fixed_pi==1 && sum_x>0))
    return {kNaN,kNaN,kNaN,ZipStatus::InvalidInput};
  if(fl || fp) {
    double l=fl?fixed_lambda:1, p=fp?fixed_pi:0;
    if(!fp) p=std::max(0.0,std::min(1.0,(r0-std::exp(-l))/(-std::expm1(-l))));
    if(!fl && p<1) {
      if(sum_x==0) return {0,p,kNaN,ZipStatus::ZeroLimit};
      // Bracket a stationary rate; compare every other bracket below.
      double lo=0,hi=sum_x/(dn-sum_zero);
      for(int i=0;i<200;++i) {
        const double mid=lo+(hi-lo)/2;
        const double w=(1-p)*std::exp(-mid)/(p+(1-p)*std::exp(-mid));
        if(sum_x/mid-(dn-sum_zero)-sum_zero*w>0)lo=mid;else hi=mid;
      }
      l=lo+(hi-lo)/2;
    }
    if(!fl && p>0 && p<1 && sum_zero>0) {
      // A fixed-inflation mixture need not be globally concave. Every
      // stationary rate lies between the ordinary and positive means.
      auto objective=[&](double t) {
        const double rate=std::exp(t);
        double out=sum_zero*std::log(p+(1-p)*std::exp(-rate));
        for(int v:x)if(v>0)out+=std::log1p(-p)+pois_detail::logpmf(v,rate);
        return out;
      };
      const double a=std::log(x_bar), b=std::log(sum_x/(dn-sum_zero));
      double best=objective(std::log(l));
      const int steps=std::max(2,static_cast<int>(std::ceil((b-a)/.125)));
      const double step=(b-a)/steps;
      for(int i=0;i<=steps;++i) {
        const double t=a+i*step;
        if(i>0 && objective(t)<objective(t-step))continue;
        if(i<steps && objective(t)<objective(t+step))continue;
        const double opt=zero_detail::maximize(objective,std::max(a,t-step),std::min(b,t+step),1e-12,200);
        const double val=objective(opt);
        if(val>best){best=val;l=std::exp(opt);}
      }
    }
    double ll=0;
    for(int v:x) ll+=v==0?std::log(p+(1-p)*std::exp(-l)):
      std::log1p(-p)+pois_detail::logpmf(v,l);
    return {l,p,ll,ZipStatus::Ok};
  }
  double lambda = 0.0;
  double pi = 0.0;
  if (r0 == 1.0) {
    // Non-identifiable: choose a finite representative of the point mass.
    return {1.0, 1.0, 0.0, ZipStatus::Ok};
  } else if (r0 > std::exp(-x_bar)) {
    const double gamma = x_bar / (1.0 - r0);
    const double z = -gamma * std::exp(-gamma);
    const double w = lambertw0(z);
    lambda = w + gamma;
    pi = (r0 - std::exp(-lambda)) / (1.0 - std::exp(-lambda));
  } else {
    pi = 0.0;
    lambda = x_bar;
  }

  if (!std::isfinite(lambda) || !std::isfinite(pi) ||
      lambda <= 0.0 || pi < 0.0 || pi >= 1.0)
    return {kNaN, kNaN, kNaN, ZipStatus::NumericalFailure};

  double logLik = 0.0;
  for (auto v : x) {
    if (v == 0) {
      logLik += std::log(pi + (1.0 - pi) * std::exp(-lambda));
    } else if (v >= 1000000) {
      logLik += std::log1p(-pi)+pois_detail::logpmf(static_cast<double>(v),lambda);
    } else {
      logLik += std::log(1.0 - pi) +
                static_cast<double>(v) * std::log(lambda) - lambda -
                cppunivariate::special::lgamma(static_cast<double>(v) + 1.0);
    }
  }
  if (!std::isfinite(logLik))
    return {kNaN, kNaN, kNaN, ZipStatus::NumericalFailure};
  return {lambda, pi, logLik, ZipStatus::Ok};
}

}  // namespace cppunivariate
