// Nakagami(shape, scale): X^2 ~ Gamma(shape, scale/shape).
#pragma once
#include "../../special/special.hpp"
#include <cmath>
namespace cppunivariate {
namespace distributions {
inline double naka_logtail(double x, double a, bool lower) {
  if (x == 0) return lower ? special::kNegInf : 0;
  if (std::isinf(x)) return lower ? 0 : special::kNegInf;
  const long double other=special::gamma_cdf_extended(x,a,!lower);
  if (other < 0.5L) return static_cast<double>(std::log1p(-other));
  const long double v=special::gamma_cdf_extended(x,a,lower);
  if (v > 0) return static_cast<double>(std::log(v));
  // Underflowed upper tail: Lentz continued fraction for Gamma(a,x).
  if (!lower && x > a) {
    double b=x+1-a, c=1e300, h=1/b, dd=h;
    for (int i=1;i<10000;++i) {
      const double an=-static_cast<double>(i)*(i-a);
      b+=2; dd=an*dd+b; if (std::abs(dd)<1e-300) dd=1e-300;
      c=b+an/c; if (std::abs(c)<1e-300) c=1e-300;
      dd=1/dd; const double delta=dd*c; h*=delta;
      if (std::abs(delta-1)<1e-15) break;
    }
    return a*std::log(x)-x-special::lgamma(a)+std::log(h);
  }
  // Underflowed lower tail: convergent incomplete-gamma power series.
  double term=1, sum=1;
  for (int i=1;i<10000;++i) {
    term*=x/(a+i); sum+=term;
    if (term<sum*1e-16) break;
  }
  return a*std::log(x)-x-special::lgamma(a+1)+std::log(sum);
}


inline bool naka_valid(double shape,double scale) {
  return std::isfinite(shape) && std::isfinite(scale) && shape>0 && scale>0;
}
inline double dnaka(double x,double shape,double scale,bool log=false) {
  if (!naka_valid(shape,scale) || std::isnan(x)) return special::kNaN;
  if (x<0 || std::isinf(x)) return log ? special::kNegInf : 0;
  double v;
  if (x==0) v=shape<0.5 ? special::kPosInf : shape>0.5 ? special::kNegInf :
    std::log(2.0)+0.5*(std::log(shape)-std::log(scale))-special::lgamma(shape);
  else {
    const double lx=std::log(x), rate=std::log(shape)-std::log(scale);
    v=std::log(2.0)+shape*rate-special::lgamma(shape)+(2*shape-1)*lx-std::exp(2*lx+rate);
  }
  return log ? v : std::exp(v);
}
inline double pnaka(double q,double shape,double scale,bool lower=true,bool log=false) {
  if (!naka_valid(shape,scale) || std::isnan(q)) return special::kNaN;
  double v;
  if (q<=0) v=lower ? special::kNegInf : 0;
  else if (std::isinf(q)) v=lower ? 0 : special::kNegInf;
  else {
    const double lx=2*std::log(q)+std::log(shape)-std::log(scale);
    if (lx < -700) {
      const double below=shape*lx-special::lgamma(shape+1);
      v=lower ? below : std::log(-std::expm1(below));
    }
    else v=naka_logtail(std::exp(lx),shape,lower);
  }
  return log ? v : std::exp(v);
}
inline double qnaka(double p,double shape,double scale,bool lower=true,bool log=false) {
  if (!naka_valid(shape,scale) || std::isnan(p) || (log ? p>0 : p<0 || p>1)) return special::kNaN;
  if (p==(log ? special::kNegInf : 0)) return lower ? 0 : special::kPosInf;
  if (p==(log ? 0 : 1)) return lower ? special::kPosInf : 0;
  double lp=log ? p : std::log(p);
  if (lp>-0.6931471805599453) { lp=std::log(-std::expm1(lp)); lower=!lower; }
  // At tiny unit-gamma quantiles, its power-law leading term is accurate
  // to better than double precision and avoids squared-value underflow.
  const double lg=(lp+special::lgamma(shape+1))/shape;
  if (lower && lg < -36)
    return std::exp(0.5*(lg+std::log(scale)-std::log(shape)));
  if (lp > -700) {
    const double g=special::qgamma(lp,shape,1,lower,true);
    if (g>0 && std::isfinite(g))
      return std::exp(0.5*(std::log(g)+std::log(scale)-std::log(shape)));
  }
  // Invert in log amplitude: squared observations may underflow/overflow.
  double lo=-745, hi=710;
  for (int i=0;i<100;++i) {
    const double mid=lo+(hi-lo)/2;
    const double f=pnaka(std::exp(mid),shape,scale,lower,true);
    if (lower ? f<lp : f>lp) lo=mid; else hi=mid;
  }
  return std::exp(lo+(hi-lo)/2);
}
template <typename Rng>
inline double rnaka(Rng &rng,double shape,double scale) {
  if (!naka_valid(shape,scale)) return special::kNaN;
  return std::sqrt(rng.rgamma(shape,1))*std::exp(0.5*(std::log(scale)-std::log(shape)));
}
}
}
