// Zero-Inflated Poisson MLE. Closed-form via Lambert W: when the empirical
// zero-rate exceeds exp(-mean), the principal branch W_0 yields the MLE.

#pragma once
#include "../../core/discrete_stop.hpp"

#include "../../core/lambert_w.hpp"
#include "../../core/span.hpp"
#include "../pois/mle.hpp"
#include "zip_certificate.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class ZipStatus { Ok = 0, InvalidInput, NumericalFailure, ZeroLimit, Budget, Tie };

struct ZipResult {
  double lambda;
  double pi;
  double logLik;
  ZipStatus status;
  StopDetail stop_detail{};
};


inline ZipResult zip_mle(ispan x,
    double fixed_lambda=std::numeric_limits<double>::quiet_NaN(),
    double fixed_pi=std::numeric_limits<double>::quiet_NaN(), int maxit=200) {
  auto stopped = [&]() -> ZipResult {
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
    if(!fp) p=std::max(0.0,std::min(1.0,1-(1-r0)/(-std::expm1(-l))));
    if(!fl && p<1) {
      if(sum_x==0) return {0,p,kNaN,ZipStatus::ZeroLimit};
      l=p==0?x_bar:sum_x/(dn-sum_zero);
    }
    if(!fl && p>0 && p<1 && sum_zero>0) {
      auto objective=[&](double rate) {
        double value=0,correction=0;
        const double a=std::log(p), b=std::log1p(-p)-rate;
        const double zero=std::max(a,b)+std::log1p(std::exp(-std::abs(a-b)));
        for(int v:x) {
          const double term=v==0?zero:std::log1p(-p)+pois_detail::logpmf(v,rate);
          const double y=term-correction,t=value+y;
          correction=(t-value)-y;value=t;
        }
        return value;
      };
      const auto pad=[&](double scale) {
        return 4096*std::numeric_limits<double>::epsilon()*(1+dn+scale);
      };
      const auto cert=zip_certificate_detail::held_rate(dn,dn-sum_zero,sum_x,p,maxit,objective,pad);
      if(cert.status!=zip_certificate_detail::Status::Ok) {
        ZipResult out{kNaN,p,kNaN,ZipStatus::NumericalFailure};
        if(cert.status==zip_certificate_detail::Status::Budget)out.status=ZipStatus::Budget;
        if(cert.status==zip_certificate_detail::Status::Tie)out.status=ZipStatus::Tie;
        out.stop_detail.budget_exhausted=out.status==ZipStatus::Budget;
        out.stop_detail.comparison_tie=out.status==ZipStatus::Tie;
        out.stop_detail.loglik_upper=cert.upper;
        for(const auto& peak:cert.peaks) {
          out.stop_detail.candidates.push_back({{peak.lo+(peak.hi-peak.lo)/2,p},peak.lower,peak.upper});
          if(std::isnan(out.stop_detail.loglik_lower) || peak.lower>out.stop_detail.loglik_lower)
            out.stop_detail.loglik_lower=peak.lower;
        }
        if(out.stop_detail.candidates.empty()) {
          const double ll=objective(x_bar),e=pad(std::abs(ll)+sum_x);
          if(std::isfinite(ll)) {
            out.stop_detail.candidates.push_back({{x_bar,p},ll-e,ll+e});
            out.stop_detail.loglik_lower=ll-e;
          }
        }
        return out;
      }
      const auto& peak=cert.peaks[cert.best];
      return {peak.lo+(peak.hi-peak.lo)/2,p,peak.ll,ZipStatus::Ok};
    }
    const double a=std::log(p),b=std::log1p(-p)-l;
    const double zero=std::max(a,b)+std::log1p(std::exp(-std::abs(a-b)));
    double ll=0;
    for(int v:x) ll+=v==0?zero:
      std::log1p(-p)+pois_detail::logpmf(v,l);
    if(!std::isfinite(ll))return {kNaN,p,kNaN,ZipStatus::NumericalFailure};
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

  }();
  if(stopped.status==ZipStatus::ZeroLimit) stopped.stop_detail=discrete_face(StopModel::Pois,{0},0);
  return stopped;
}

}  // namespace cppunivariate
