// Beta negative binomial: stable exact masses and budgeted integer inversion.
#pragma once
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>

namespace cppunivariate {
namespace bnbinom_detail {
inline long double stirling(long double z) {
  const long double t = 1/z, t2 = t*t;
  return t*(1.L/12-t2*(1.L/360-t2*(1.L/1260-t2*(1.L/1680-t2/1188))));
}
// log Gamma(z+h)/Gamma(z), avoiding subtraction of large log gammas.
inline long double increment(long double z, long double h) {
  long double correction = 0;
  while (z < 16) { correction -= std::log1p(h/z); z += 1; }
  return correction + h*std::log(z) + (z+h-.5L)*std::log1p(h/z)-h +
    stirling(z+h)-stirling(z);
}
inline long double zero(long double r, long double a, long double b) {
  if (r > b) std::swap(r,b);
  long double correction = 0;
  while (a < 16) {
    correction += std::log(a/(a+r)) + std::log1p(r/(a+b));
    a += 1;
  }
  return correction - r*std::log1p(b/a) +
    (a+r-.5L)*std::log1p(r/a)-(a+b+r-.5L)*std::log1p(r/(a+b)) +
    stirling(a+r)-stirling(a)-stirling(a+b+r)+stirling(a+b);
}
inline double logpmf(int x, double r, double a, double b) {
  return static_cast<double>(zero(r,a,b)+increment(r,x)+increment(b,x)-
    increment(static_cast<long double>(a)+r+b,x)-special::lgamma(x+1.0));
}
inline bool valid(double r, double a, double b) {
  return r>0 && a>0 && b>0 && std::isfinite(r) && std::isfinite(a) && std::isfinite(b);
}
}
namespace distributions {
inline double dbnbinom(double x, double size, double alpha, double beta, bool log) {
  if (!bnbinom_detail::valid(size,alpha,beta) || std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  if (x<0 || !std::isfinite(x) || x!=std::floor(x))
    return log ? -std::numeric_limits<double>::infinity() : 0;
  if (x>std::numeric_limits<int>::max()) return std::numeric_limits<double>::quiet_NaN();
  const double l = bnbinom_detail::logpmf(static_cast<int>(x),size,alpha,beta);
  return log ? l : std::exp(l);
}
// O(q); a finite work budget is part of this interface (NaN on exhaustion).
inline double pbnbinom(double q, double size, double alpha, double beta,
                       bool lower_tail, bool log_p) {
  if (!bnbinom_detail::valid(size,alpha,beta) || std::isnan(q))
    return std::numeric_limits<double>::quiet_NaN();
  if (q==std::numeric_limits<double>::infinity())
    return lower_tail ? (log_p ? 0 : 1) : (log_p ? -q : 0);
  if (q>1000000) return std::numeric_limits<double>::quiet_NaN();
  long double mass=std::exp(bnbinom_detail::zero(size,alpha,beta)), sum=0;
  for (int j=0; j<=q; ++j) {
    sum+=mass;
    mass*=((static_cast<long double>(size)+j)/(j+1.L))*
      ((static_cast<long double>(beta)+j)/(static_cast<long double>(alpha)+size+beta+j));
  }
  sum=std::min(1.L,sum);
  return static_cast<double>(lower_tail ? (log_p ? std::log(sum) : sum) :
    (log_p ? std::log1p(-sum) : 1-sum));
}
inline double qbnbinom(double p, double size, double alpha, double beta,
                       bool lower_tail, bool log_p) {
  if (!bnbinom_detail::valid(size,alpha,beta) || std::isnan(p) ||
      (log_p ? p>0 : p<0 || p>1)) return std::numeric_limits<double>::quiet_NaN();
  const long double u=log_p ? (lower_tail ? std::exp(p) : -std::expm1(p)) :
    (lower_tail ? p : 1.L-p);
  if (u==0) return 0;
  if (u==1) return std::numeric_limits<double>::infinity();
  long double mass=std::exp(bnbinom_detail::zero(size,alpha,beta)), sum=0;
  for (int j=0; j<=1000000; ++j) {
    sum+=mass;
    if (sum>=u) return j;
    mass*=((static_cast<long double>(size)+j)/(j+1.L))*
      ((static_cast<long double>(beta)+j)/(static_cast<long double>(alpha)+size+beta+j));
  }
  return std::numeric_limits<double>::quiet_NaN();
}
template <typename Rng>
inline double rbnbinom(Rng &rng, double size, double alpha, double beta) {
  return rng.rpois(rng.rgamma(size,1)*rng.rgamma(beta,1)/rng.rgamma(alpha,1));
}
}
}
