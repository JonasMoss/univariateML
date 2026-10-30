#pragma once
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace sgomp_detail {
inline double nan() { return std::numeric_limits<double>::quiet_NaN(); }
inline double inf() { return std::numeric_limits<double>::infinity(); }
inline bool valid(double b,double eta) { return b>0 && std::isfinite(b) && eta>=0 && std::isfinite(eta); }
inline double softplus(double x) { return std::max(x,0.0)+std::log1p(std::exp(-std::abs(x))); }
inline double logadd(double a,double b) {
  if (a==-inf()) return b;
  if (b==-inf()) return a;
  return std::max(a,b)+std::log1p(std::exp(-std::abs(a-b)));
}
// log(1-exp(-exp(t))), including exp(t) underflow.
inline double log_event(double t) {
  if (t < -36) return t;
  if (t > 7) return 0;
  return std::log(-std::expm1(-std::exp(t)));
}
inline double logcdf_z(double z,double logz,double eta) {
  return log_event(logz)-eta*std::exp(-z);
}
inline double logsurv_z(double z,double logz,double eta) {
  if (eta==0) return -z;
  // S = v + (1-v)*(1-exp(-eta*v)); neither tail is subtracted.
  return logadd(-z,log_event(logz)+log_event(std::log(eta)-z));
}
}
namespace distributions {
inline double dsgomp(double x,double b,double eta,bool log=false) {
  if (!sgomp_detail::valid(b,eta) || std::isnan(x)) return sgomp_detail::nan();
  if (x<0 || std::isinf(x)) return log ? -sgomp_detail::inf() : 0;
  const double z=b*x;
  const double a=-std::expm1(-z);
  const double extra=eta==0 || a==0 ? 0 : sgomp_detail::softplus(std::log(eta)+std::log(a));
  const double ld=std::log(b)-z-eta*std::exp(-z)+extra;
  return log ? ld : std::exp(ld);
}
inline double psgomp(double q,double b,double eta,bool lower_tail=true,bool log_p=false) {
  if (!sgomp_detail::valid(b,eta) || std::isnan(q)) return sgomp_detail::nan();
  double lp;
  if (q<=0) lp=lower_tail ? -sgomp_detail::inf() : 0;
  else if (std::isinf(q)) lp=lower_tail ? 0 : -sgomp_detail::inf();
  else {
    const double z=b*q, lz=std::log(b)+std::log(q);
    lp=lower_tail ? sgomp_detail::logcdf_z(z,lz,eta) : sgomp_detail::logsurv_z(z,lz,eta);
  }
  return log_p ? lp : std::exp(lp);
}
inline double qsgomp(double p,double b,double eta,bool lower_tail=true,bool log_p=false) {
  if (!sgomp_detail::valid(b,eta) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return sgomp_detail::nan();
  double lp=log_p ? p : std::log(p);
  if (lp==-sgomp_detail::inf()) return lower_tail ? 0 : sgomp_detail::inf();
  if (lp==0) return lower_tail ? sgomp_detail::inf() : 0;
  // Invert the smaller tail, retaining log probabilities throughout.
  if (lp > -0.6931471805599453) { lp=std::log(-std::expm1(lp)); lower_tail=!lower_tail; }
  if (eta==0) {
    if (lower_tail && lp < -36) return std::exp(lp-std::log(b));
    const double z=lower_tail ? -std::log1p(-std::exp(lp)) : -lp;
    return z/b;
  }
  // A bracket in log(b*x) also resolves probabilities below DBL_MIN.
  // b and a representable x can each be as small as exp(-745).
  // A fixed lower bracket avoids enormous intervals when log(p) and eta
  // are both huge but their quantile is ordinary.
  double lo=-1500,hi=std::log(std::numeric_limits<double>::max());
  for (int i=0;i<220;++i) {
    const double m=(lo+hi)/2,z=std::exp(m);
    const double value=lower_tail ? sgomp_detail::logcdf_z(z,m,eta) : sgomp_detail::logsurv_z(z,m,eta);
    if ((value<lp)==lower_tail) lo=m; else hi=m;
  }
  return std::exp((lo+hi)/2-std::log(b));
}
template <typename Rng>
inline double rsgomp(Rng &rng,double b,double eta) {
  if (!sgomp_detail::valid(b,eta)) return sgomp_detail::nan();
  const double e=-std::log(rng.runif(0,1));
  if (eta==0) return e/b;
  const double g=std::log(eta)-std::log(-std::log(rng.runif(0,1)));
  return std::max(e,g)/b;
}
}
}
