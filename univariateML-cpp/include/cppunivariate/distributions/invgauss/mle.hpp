// Inverse Gaussian MLE: closed form.
//   mu = mean(x)
//   1/lambda = mean(1/x) - 1/mu

#pragma once
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "dpqr.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <initializer_list>
#include <limits>

namespace cppunivariate {

enum class InvGaussStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct InvGaussResult {
  double mu;
  double lambda;
  double logLik;
  InvGaussStatus status = InvGaussStatus::Ok;
};

inline InvGaussResult invgauss_mle(span x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, InvGaussStatus::InvalidInput};
  for (double v : x) if (!(v > 0 && std::isfinite(v)))
    return {kNaN,kNaN,kNaN,InvGaussStatus::InvalidInput};

  const double dn = static_cast<double>(n);
  CPPUNIV_EXTENDED_REAL total = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (auto v : x) total += v;
  double mu = static_cast<double>(total / dn);
  double lambda;
  CPPUNIV_EXTENDED_REAL relative_ss = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (auto v : x) {
    const CPPUNIV_EXTENDED_REAL y = static_cast<CPPUNIV_EXTENDED_REAL>(v) / mu;
    relative_ss += (y - CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)) * (y - CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)) / y;
  }
  lambda = static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(mu) * dn / relative_ss);
  if (relative_ss == 0) return {mu,lambda,INFINITY,InvGaussStatus::Unbounded};
  // The profiled quadratic equals n in exact arithmetic, but not after
  // rounding mu and lambda. Evaluate the returned public point explicitly.
  const auto likelihood=[&](double m,double s) {
    double value=0,correction=0;
    for(double v:x) {
      const double term=distributions::dinvgauss(v,m,s,true)-correction;
      const double next=value+term;correction=(next-value)-term;value=next;
    }
    return value;
  };
  double logLik=likelihood(mu,lambda);
#ifdef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
  // The raw sum can round a concentrated sample mean one ulp away from
  // its nearest representation. Resolve the shape at the continuous
  // centered mean; Q is unimodal in each neighboring public mean.
  if(std::isfinite(mu) && std::isfinite(logLik)) {
    double centered=0,correction=0;
    for(double v:x) {
      const double term=(v-mu)-correction,next=centered+term;
      correction=(next-centered)-term;centered=next;
    }
    const double offset=centered/dn,center=mu+offset;
    const auto residual_sum=[&](double origin,double shift,double denominator) {
      double q=0,carry=0;
      for(double v:x) {
        const double d=((v-origin)-shift)/denominator;
        const double term=d*d/v-carry,next=q+term;
        carry=(next-q)-term;q=next;
      }
      return q;
    };
    const double q0=residual_sum(mu,0,mu),qstar=residual_sum(mu,offset,center);
    const double s=dn/qstar;
    double best_mu=mu,best_shape=lambda,best_ll=logLik;
    for(double m:{std::nextafter(center,0.0),center,
        std::nextafter(center,std::numeric_limits<double>::infinity())}) {
      if(!(m>0 && std::isfinite(m)))continue;
      if(!(s>0 && std::isfinite(s)))continue;
      const double value=likelihood(m,s);
      if(value>best_ll) {best_mu=m;best_shape=s;best_ll=value;}
    }
    // The exact profiled gap is n/2 * [u-1-log(u)+log(Q(mu)/Q(mean))],
    // u=lambda*Q(mu)/n. Use the centered mean offset for the Q ratio:
    // Q(mu)-Q(mean)=n*offset^2/(mean*mu^2), without subtracting close sums.
    double gap=best_ll-logLik;
    const double u=lambda*q0/dn,relative_offset=offset/mu;
    const double mean_ratio=dn/(center*qstar)*relative_offset*relative_offset;
    if(u>0 && std::isfinite(u) && mean_ratio>=0 && std::isfinite(mean_ratio)) {
      const double d=u-1;
      gap=std::max(gap,.5*dn*(d-std::log1p(d)+std::log1p(mean_ratio)));
    }
    if(gap>1e-10*std::max(1.0,std::abs(best_ll))) {
      mu=best_mu;lambda=best_shape;logLik=best_ll;
    }
  }
#endif
  return {mu, lambda, logLik,
    std::isfinite(mu) && std::isfinite(lambda) && std::isfinite(logLik) ?
      InvGaussStatus::Ok : InvGaussStatus::IterationFailure};
}

inline InvGaussResult invgauss_mle(span x,double mu,double shape) {
  if(std::isnan(mu) && std::isnan(shape))return invgauss_mle(x);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0 || (!std::isnan(mu) && !(mu>0 && std::isfinite(mu))) ||
     (!std::isnan(shape) && !(shape>0 && std::isfinite(shape))))return {nan,nan,nan,InvGaussStatus::InvalidInput};
  CPPUNIV_EXTENDED_REAL total=0,logs=0;
  for(double v:x){if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,InvGaussStatus::InvalidInput};total+=v;logs+=std::log(v);}
  const double n=static_cast<double>(x.size());
  if(std::isnan(mu))mu=static_cast<double>(total/n);
  CPPUNIV_EXTENDED_REAL q=0;for(double v:x){const CPPUNIV_EXTENDED_REAL d=(static_cast<CPPUNIV_EXTENDED_REAL>(v)-mu)/mu;q+=d*d/v;}
  if(std::isnan(shape) && q==0)return {mu,nan,nan,InvGaussStatus::Unbounded};
  if(std::isnan(shape))shape=static_cast<double>(n/q);
  const double ll=static_cast<double>(0.5*n*(std::log(shape)-std::log(2*cppunivariate::pi))-1.5*logs-0.5*shape*q);
  return {mu,shape,ll};
}

}  // namespace cppunivariate
