// Inverse Gaussian MLE: closed form.
//   mu = mean(x)
//   1/lambda = mean(1/x) - 1/mu

#pragma once
#include "../../core/precision.hpp"

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
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
  if (n == 0) return {0.0, 0.0, kNaN};

  const double dn = static_cast<double>(n);
  double sum_log = 0.0;
  for (auto v : x) {
    sum_log += std::log(v);
  }
  CPPUNIV_EXTENDED_REAL total = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (auto v : x) total += v;
  const double mu = static_cast<double>(total / dn);
  double lambda;
  CPPUNIV_EXTENDED_REAL relative_ss = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (auto v : x) {
    const CPPUNIV_EXTENDED_REAL y = static_cast<CPPUNIV_EXTENDED_REAL>(v) / mu;
    relative_ss += (y - CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)) * (y - CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)) / y;
  }
  lambda = static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(mu) * dn / relative_ss);
  const double L = sum_log / dn;

  // At the profiled lambda, the quadratic term equals n exactly.
  const double logLik = -0.5 * dn *
      (3.0 * L - std::log(lambda) + std::log(2.0 * M_PI) + 1.0);
  return {mu, lambda, logLik};
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
  const double ll=static_cast<double>(0.5*n*(std::log(shape)-std::log(2*M_PI))-1.5*logs-0.5*shape*q);
  return {mu,shape,ll};
}

}  // namespace cppunivariate
