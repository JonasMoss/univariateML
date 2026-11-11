// Centered log-gamma tail quadrature, shared with the Prentice gamma kernel.
// Exact-arithmetic Simpson and tangent bounds are documented in
// dev/research/evaluations/2026-11-10-gg-dpqr-report.md.
#pragma once
#include "constants.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate { namespace special { namespace gamma_centered {
constexpr double half_log_two_pi = 0.91893853320467274178;

// (exp(z)-1-z)/z^2, including its limit.
inline double expm1_remainder_ratio(double z) {
  if (std::abs(z) >= 0.1) return ((std::expm1(z)-z)/z)/z;
  double term=0.5, sum=term;
  for (int k=3;k<=24;++k) { term*=z/k; sum+=term; }
  return sum;
}
inline double exprel(double z) {
  return z == 0 ? 1 : std::expm1(z)/z;
}
inline double penalty(double w,double shape) {
  return (w*expm1_remainder_ratio(shape*w))*w;
}
inline double remainder(double shape) {
  const double q=std::abs(shape), s=q*q;
  if (q <= 0.25)
    return s*(1.0/12-s*s/360+s*s*s*s/1260-s*s*s*s*s*s/1680);
  const double a=(1/q)/q;
  if (a==0) return std::log(q)-half_log_two_pi;
  return std::lgamma(a)-(a-0.5)*std::log(a)+a-half_log_two_pi;
}
struct TailIntegrand {
  double w, shape, rate, direction, slope, ew;
  double delta(double y) const {
    const double d=direction*y/rate, z=shape*d;
    return direction*y*(slope/rate)+ew*(d*expm1_remainder_ratio(z))*d;
  }
  double value(double y) const { return std::exp(-delta(y)); }
  double gradient(double y) const {
    const double d=direction*y/rate;
    return direction*(slope+ew*d*exprel(shape*d))/rate;
  }
  double curvature(double y) const {
    return (std::exp(shape*(w+direction*y/rate))/rate)/rate;
  }
};
inline double integrate(const TailIntegrand &f,double a,double b,double fa,double fm,double fb,
                        double tol,bool &ok) {
  if (!ok) return 0;
  const double width=b-a;
  const double m1=std::max(std::abs(f.gradient(a)),std::abs(f.gradient(b)));
  const double m2=std::max(f.curvature(a),f.curvature(b));
  const double c=std::abs(f.shape)/f.rate, m12=m1*m1;
  const double bound=fa*(m12*m12+6*m12*m2+3*m2*m2+4*m1*m2*c+m2*c*c)*
      width*width*width*width*width/2880;
  if (std::isfinite(bound) && bound<=tol) return width*(fa+4*fm+fb)/6;
  const double mid=(a+b)/2;
  if (mid==a || mid==b || tol==0) { ok=false; return 0; }
  const double left=integrate(f,a,mid,fa,f.value((a+mid)/2),fm,tol/2,ok);
  const double right=integrate(f,mid,b,fm,f.value((mid+b)/2),fb,tol/2,ok);
  return left+right;
}
inline double log_tail(double w,double shape) {
  if (std::isinf(w)) return special::kNegInf;
  const double z=shape*w, slope=w*exprel(z), ew=std::exp(z);
  const double rate=std::max(1.0,std::abs(slope));
  if (!std::isfinite(rate) || !std::isfinite(ew)) return special::kNegInf;
  const TailIntegrand f{w,shape,rate,w<0 ? -1.0 : 1.0,slope,ew};
  double end=1;
  for (;;) {
    const double g=f.gradient(end), v=f.value(end);
    if (g>0 && v/g < 2.5e-14) break;
    end*=2;
    if (!std::isfinite(end)) return special::kNaN;
  }
  bool ok=true;
  const double integral=integrate(f,0,end,1,f.value(end/2),f.value(end),5e-14,ok)/rate;
  if (!ok || !(integral>0)) return special::kNaN;
  return -half_log_two_pi-remainder(shape)-penalty(w,shape)+std::log(integral);
}

} } }
