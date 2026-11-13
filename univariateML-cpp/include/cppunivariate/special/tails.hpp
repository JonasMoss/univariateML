// Contract, retained choices, evidence and known numerical limitations:
// dev/research/evaluations/2026-10-31-step7-routines.md (repository root).
// Tail-accurate d/p/q rules: compute the smaller tail directly in log space;
// derive its complement with log1mexp, never 1-p near p=1. Quantiles use
// the accurate log tail, including log inputs -1e4 and below; do not first
// exponentiate them. Handle p=0/1 (log p=-Inf/0) at support endpoints.
// Discrete quantiles use discrete_reached below for left-continuity fuzz.
// They return the first adjusted crossing on the integer lattice while
// k <= 2^53; beyond, the smallest representable double crossing
// (relative lattice spacing <= 2^-52; the fuzz can cross multiple points).
// NaN only for invalid input or a failed CDF evaluation.
// Preserve logarithms of underflowed intermediates analytically when needed.
// Double only: guard range with log-space algebra; CPPUNIV_EXTENDED_REAL equals
// double on some CRAN platforms. Extended precision may improve rounding,
// but must never be required to keep an intermediate in range.
// Example: Lomax log_upper=-kappa*log1p_product(lambda,x);
// q=expm1(-log_upper/kappa)/lambda (guard intermediate overflow).
#pragma once
#include "../core/precision.hpp"
#include "constants.hpp"
#include "gamma_centered.hpp"
#include <cmath>
#include <algorithm>
#include <array>

namespace cppunivariate {
namespace special {

// Domain x <= 0; endpoints -Inf -> 0, 0 -> -Inf; invalid -> NaN.
template <typename Real>
inline auto log1mexp(Real x) -> decltype(std::log(x)) {
  if (!(x <= 0.0)) return kNaN;
  return x > -0.69314718055994530942 ? std::log(-std::expm1(x))
                                     : std::log1p(-std::exp(x));
}
inline double log_sum_exp(double x, double y) {
  if (std::isnan(x) || std::isnan(y)) return kNaN;
  if (x == kPosInf || y == kPosInf) return kPosInf;
  if (x == kNegInf) return y;
  if (y == kNegInf) return x;
  const double hi = x > y ? x : y;
  return hi + std::log1p(std::exp(-std::fabs(x-y)));
}
// log(exp(x)-exp(y)); equal finite inputs (also -Inf) give -Inf.
// Inf-Inf and y>x are undefined.
inline double log_diff_exp(double x, double y) {
  if (std::isnan(x) || std::isnan(y) || y > x || y == kPosInf) return kNaN;
  if (x == y) return kNegInf;
  if (y == kNegInf || x == kPosInf) return x;
  return x + log1mexp(y-x);
}
// log(-log(1-exp(x))) without underflowing exp(x). The omitted relative
// correction below -36 is < 1.2e-16. Domain x <= 0; -Inf -> -Inf,
// 0 -> +Inf; invalid -> NaN.
template <typename Real>
inline auto log_neg_log1mexp(Real x) -> decltype(std::log(x)) {
  if (!(x <= 0.0)) return kNaN;
  return x < -36.0 ? x : std::log(-log1mexp(x));
}

// Positive finite factors (zero/infinity also obey their usual limits).
// Keep the product in log space when multiplication overflows.
inline double log1p_product(double a, double b) {
  if (b > 1.0 && a >= std::numeric_limits<double>::max() / b)
    return std::log(a) + std::log(b);
  return std::log1p(a * b);
}
// log(log(1+a*b)), retaining a product below the subnormal range.
inline double log_log1p_product(double a, double b) {
  const double product = a * b;
  return product < std::numeric_limits<double>::min()
      ? std::log(a) + std::log(b) : std::log(log1p_product(a, b));
}
// Standardization with finite location and positive finite scale. If the
// subtraction overflows, the operands have opposite signs: dividing first
// avoids that overflow without introducing cancellation or Inf-Inf.
inline double scaled_difference(double x, double location, double scale) {
  if (!std::isfinite(x)) return x;
  const double difference = x - location;
  return std::isfinite(difference) ? difference / scale
                                 : x / scale - location / scale;
}

// Probability minus one half, for quantiles near the median. The low part
// of log(2) retains the offset of a rounded log(0.5) input from the true median.
// Do not use this centered representation to reconstruct extreme tails.
inline double probability_minus_half(double p, bool lower_tail, bool log_p) {
  const double offset = log_p ? 0.5 * std::expm1(
      (p + 0.69314718055994528623) + 2.3190468138462995584e-17) : p - 0.5;
  return lower_tail ? offset : -offset;
}

// Preserve relative differences near equal positive arguments.
inline double log_positive_ratio(double a, double b) {
  return a > b * 0.5 && a * 0.5 < b
      ? std::log1p((a-b)/b) : std::log(a)-std::log(b);
}
// Low part of an addition (FastTwoSum with magnitude ordering).
inline double addition_residual(double a, double b, double sum) {
  return std::abs(a)>=std::abs(b) ? (a-sum)+b : (b-sum)+a;
}
// Keep the center location+scale before forming a concentrated log ratio.
inline double log_shifted_positive_ratio(double x, double location, double scale) {
  const double center=location+scale;
  if (std::isfinite(center)) {
    const double delta=x-center;
    if (std::abs(delta)<=scale*0.5) {
      const double low=addition_residual(location,scale,center);
      return std::log1p((delta-low)/scale);
    }
  }
  return log_positive_ratio(x-location,scale);
}
inline double shifted_scaled_exp(double location, double scale, double displacement) {
  const double center=location+scale;
  const double offset=std::expm1(displacement);
  if (displacement>=-0.6931471805599453 && std::isfinite(center) && std::isfinite(offset))
    return std::fma(scale,offset,center)+addition_residual(location,scale,center);
  return location+std::exp(std::log(scale)+displacement);
}
// Do not underflow log_power/power before multiplying by a huge scale.
inline double shifted_scaled_power_quantile(double location,double scale,
                                            double log_power,double power) {
  const double displacement=log_power/power;
  if (std::isfinite(displacement) && std::expm1(displacement)==displacement) {
    const double center=location+scale;
    if (std::isfinite(center)) {
      const double low=addition_residual(location,scale,center);
      if (log_power==0) return center+low;
      const double ratio=scale/power;
      if (ratio>=std::numeric_limits<double>::min() && std::isfinite(ratio))
        return std::fma(ratio,log_power,center)+low;
      const double offset=std::copysign(std::exp(std::log(scale)+std::log(std::abs(log_power))-std::log(power)),log_power);
      return (center+offset)+low;
    }
  }
  return shifted_scaled_exp(location,scale,displacement);
}
// offset is p-1/2; preserve a small result when finite endpoints cancel.
inline double centered_interval_quantile(double lo, double hi, double offset) {
  const double a=lo*0.5, b=hi*0.5, center=a+b, radius=b-a;
  const double low=addition_residual(a,b,center);
  const double radius_low=addition_residual(b,-a,radius);
  const double value=std::fma(radius,2*offset,center)+(low+radius_low*(2*offset));
  return std::isnan(value)?value:std::max(lo,std::min(hi,value));
}
// Stable log(1+exp(x)) and its logarithm, including underflowed exp(x).
inline double softplus(double x) {
  return x > 0.0 ? x + std::log1p(std::exp(-x)) : std::log1p(std::exp(x));
}
inline double log_softplus(double x) {
  return x < -36.0 ? x : std::log(softplus(x));
}
// log(log(1+exp(a*b))), with a positive. A large positive product may
// overflow even when a later shape factor rescues the resulting hazard.
inline double log_softplus_product(double a, double b) {
  // Guard before multiplying: GCC 15 can infer b=Inf in an a*b==Inf
  // branch after inlining a finite-a check, losing the finite log(b).
  // In this range softplus(a*b) rounds to a*b even at the finite boundary.
  if (b > 1.0 && a >= std::numeric_limits<double>::max() / b)
    return std::log(a) + std::log(b);
  return log_softplus(a*b);
}
// log(expm1(exp(lv))) without forming an overflowing or underflowing expm1.
inline double log_expm1_from_log(double lv) {
  if (lv < -36.0) return lv;
  const double v = std::exp(lv);
  return v == kPosInf ? v : v + log1mexp(-v);
}

// Logarithm of a distance, even when the subtraction overflows.
// Student location/scale and standardized-GED tails use this before scaling.
inline double log_abs_difference(double x, double location) {
  const double d=x-location;
  return std::isfinite(d) ? std::log(std::abs(d))
      : std::log(std::abs(x/2-location/2))+std::log(2.0);
}

// (scale + shape*(x-location))/scale, retaining the subtraction residual.
// Used by GEV and GPD at finite support endpoints.
inline double support_residual(double x, double location, double scale, double shape) {
  if (shape == 0) return 1;
  const double d = x-location;
  if (!std::isfinite(d)) return 1+shape*scaled_difference(x,location,scale);
  const double v = d-x;
  const double error = (x-(d-v))-(location+v);
  return (std::fma(shape,d,scale)+shape*error)/scale;
}

struct LogTails {
  double lower;
  double upper;
  bool valid;
};
inline LogTails tails_from_log(double log_tail, bool lower_tail) {
  if (!(log_tail <= 0.0)) return {kNaN, kNaN, false};
  const double other = log1mexp(log_tail);
  return lower_tail ? LogTails{log_tail, other, true}
                    : LogTails{other, log_tail, true};
}
// One tail is exp(-h). Keep log(h) until choosing the smaller tail so a
// tiny shape/hazard can still supply an accurate log complement.
inline LogTails tails_from_log_hazard(double log_h, bool exp_tail_lower) {
  return log_h < -36.0 ? tails_from_log(log_h, !exp_tail_lower)
                      : tails_from_log(-std::exp(log_h), exp_tail_lower);
}
// Supply both accurately computed logs, or construct them from the smaller
// tail with tails_from_log. This function never subtracts probabilities.
inline double tail_probability(const LogTails &tails, bool lower_tail, bool log_p) {
  if (!tails.valid) return kNaN;
  const double lp = lower_tail ? tails.lower : tails.upper;
  return log_p ? lp : std::exp(lp);
}
// No support/rounding semantics here: discrete callers still own flooring
// and smallest-support-point inversion, continuous callers own endpoints.
inline LogTails quantile_log_tails(double p, bool lower_tail, bool log_p) {
  if (log_p) return tails_from_log(p, lower_tail);
  if (!(p >= 0.0 && p <= 1.0)) return {kNaN, kNaN, false};
  const double lp = std::log(p);
  const double lq = std::log1p(-p);
  return lower_tail ? LogTails{lp, lq, true} : LogTails{lq, lp, true};
}


// log Gamma(x+u)/Gamma(x+v), with positive shifted arguments.
// Stirling through z^-11 at z>=16; omitted term < 3e-17 per gamma.
// Small relative shifts use a series for z*log1p(h/z)-h, avoiding
// subtraction and retaining the limiting power law at DBL_MAX.
inline double log_gamma_ratio(double x, double u, double v) {
  double z=x+v, h=u-v, correction=0;
  if (!(z>0) || !(x+u>0) || !std::isfinite(z) || !std::isfinite(h)) return kNaN;
  // At most 16 unit shifts put positive arguments in the Stirling range.
  // Cancellation in z+h can otherwise leave a huge z stuck below that test.
  for (int i=0; (z<16.0 || z+h<16.0) && i<17; ++i) {
    if (z+1==z || !(z+h>0)) return kNaN;
    correction+=std::log(z)-std::log(z+h); z+=1.0;
  }
  if (z<16.0 || z+h<16.0) return kNaN;
  const auto remainder=[](double y) {
    const double t=1.0/y, t2=t*t;
    return t*(1.0/12-t2*(1.0/360-t2*(1.0/1260-t2*(1.0/1680-t2*(1.0/1188-t2*691.0/360360)))));
  };
  const double t=h/z, l=std::log1p(t);
  double residual;
  if(std::abs(t)<0.01) {
    double power=t, total=0;
    for(int j=2;j<=12;++j) {total+=(j%2 ? 1.0 : -1.0)*power/j;power*=t;}
    residual=h*total;
  } else residual=z*l-h;
  return correction+h*std::log(z)+residual+(h-.5)*l+remainder(z+h)-remainder(z);
}

// Stirling log-gamma remainder through z^-11; the next term at z>=16
// is below 3e-17. This normalizer is used only by probability evaluation.
inline double density_stirling_remainder(double z) {
  const double t=1/z, t2=t*t;
  return t*(1.0/12-t2*(1.0/360-t2*(1.0/1260-t2*(1.0/1680-t2*(1.0/1188-t2*691.0/360360)))));
}
// shape*(u-log1p(u)), preserving shape*u^2 even if u^2 underflows.
inline double scaled_positive_deviance(double shape,double u) {
  if (std::abs(u)<=0.5) {
    // Finite Taylor polynomial: remainder <= |u|^65/(66*(1-|u|)).
    double coefficient=1.0/66;
    for (int j=65;j>=2;--j) coefficient=1.0/j-u*coefficient;
    return ((shape*u)*u)*coefficient;
  }
  return shape*(u-std::log1p(u));
}
inline double gamma_log_density_positive(double x,double shape,double scale) {
  const double lx=std::log(x);
  if (shape<16)
    return shape*log_positive_ratio(x,scale)-lx-x/scale-std::lgamma(shape);
  int scale_exponent=0, shape_exponent=0;
  const double ms=std::frexp(scale,&scale_exponent);
  const double mk=std::frexp(shape,&shape_exponent);
  const double mean=ms*mk, low=std::fma(ms,mk,-mean);
  const double normalized=std::scalbn(x,-(scale_exponent+shape_exponent));
  double penalty;
  if (normalized>=mean*0.5 && normalized<=mean*2) {
    const double u=((normalized-mean)-low)/mean;
    penalty=scaled_positive_deviance(shape,u);
  } else {
    if (std::isinf(normalized)) return kNegInf;
    const double lr=normalized>=std::numeric_limits<double>::min()
        ? std::log(normalized)-std::log(mean)
        : lx-std::log(scale)-std::log(shape);
    penalty=shape*(std::expm1(lr)-lr);
  }
  return 0.5*std::log(shape)-lx-0.91893853320467274178
      -density_stirling_remainder(shape)-penalty;
}

// Positive beta normalizer without subtracting large log gamma values.
// The shifted gamma ratio bounds its Stirling remainder by 3e-17.
inline double log_beta_positive(double a, double b) {
  if (!(a>0 && b>0 && std::isfinite(a) && std::isfinite(b))) return kNaN;
  if (a==1) return -std::log(b);
  if (b==1) return -std::log(a);
  const double small=std::min(a,b), large=std::max(a,b);
  if (small<16) return std::lgamma(small)-log_gamma_ratio(large,small,0);
  const auto remainder=[](double y) {
    const double t=1.0/y, t2=t*t;
    return t*(1.0/12-t2*(1.0/360-t2*(1.0/1260-t2*(1.0/1680-t2*(1.0/1188-t2*691.0/360360)))));
  };
  const double logsum=std::log(large)+std::log1p(small/large);
  return -(a-.5)*std::log1p(b/a)-(b-.5)*std::log1p(a/b)
      -.5*logsum+0.91893853320467274178
      +remainder(a)+remainder(b)-remainder(a+b);
}

// Exact expansion of x*(a+b)-a, apart from underflowed low products.
// All products are bounded by one after power-of-two normalization.
inline double beta_center_residual(double x,double a,double b) {
  const double p=x*a, q=x*b;
  const std::array<double,5> terms{{p,std::fma(x,a,-p),q,std::fma(x,b,-q),-a}};
  std::array<double,6> expansion{};
  std::size_t length=0;
  for (double term:terms) {
    double carry=term;
    for (std::size_t i=0;i<length;++i) {
      const double sum=carry+expansion[i];
      expansion[i]=addition_residual(carry,expansion[i],sum);
      carry=sum;
    }
    expansion[length++]=carry;
  }
  double value=0;
  for (std::size_t i=0;i<length;++i) value+=expansion[i];
  return value;
}
inline double beta_log_density_positive(double x,double a,double b) {
  const double lx=std::log(x), ly=std::log1p(-x);
  if (a<16 || b<16)
    return (a-1)*lx+(b-1)*ly-log_beta_positive(a,b);
  int ea=0, eb=0;
  std::frexp(a,&ea);std::frexp(b,&eb);
  const int exponent=std::max(ea,eb);
  const double na=std::scalbn(a,-exponent), nb=std::scalbn(b,-exponent);
  const double delta=beta_center_residual(x,na,nb);
  const double u=delta/na, v=-delta/nb;
  const double logsum=std::log(std::max(a,b))+std::log1p(std::min(a,b)/std::max(a,b));
  const double first=std::abs(u)<=0.5 ? scaled_positive_deviance(a,u)
      : a*(u-(lx+logsum-std::log(a)));
  const double second=std::abs(v)<=0.5 ? scaled_positive_deviance(b,v)
      : b*(v-(ly+logsum-std::log(b)));
  const double normalizer=0.5*(std::log(a)+std::log(b)-logsum)-0.91893853320467274178
      -density_stirling_remainder(a)-density_stirling_remainder(b)+density_stirling_remainder(a+b);
  return normalizer-lx-ly-first-second;
}

inline double log_positive_distance(double hi,double lo) {
  const double d=hi-lo;
  return std::isfinite(d)?std::log(d):std::log(hi*0.5-lo*0.5)+0.6931471805599453;
}
// Log coordinates of (x-lo)/(hi-lo), retaining a tiny distance to an edge.
inline LogTails interval_log_coordinates(double x,double lo,double hi) {
  double left=x-lo, right=hi-x;
  if(!std::isfinite(left) || !std::isfinite(right)) {
    left=x*0.5-lo*0.5;right=hi*0.5-x*0.5;
  }
  if(left<=right) {
    const double ratio=left/right;
    const double lr=ratio>=std::numeric_limits<double>::min() ? std::log(ratio)
        : log_positive_distance(x,lo)-log_positive_distance(hi,x);
    const double upper=-std::log1p(ratio);
    return {lr+upper,upper,true};
  }
  const double ratio=right/left;
  const double lr=ratio>=std::numeric_limits<double>::min() ? std::log(ratio)
      : log_positive_distance(hi,x)-log_positive_distance(x,lo);
  const double lower=-std::log1p(ratio);
  return {lower,lr+lower,true};
}
inline double affine_beta_log_density_positive(double x,double a,double b,double lo,double hi) {
  const double width_log=log_positive_distance(hi,lo);
  const auto coordinates=interval_log_coordinates(x,lo,hi);
  if(a<16 || b<16)
    return (a-1)*coordinates.lower+(b-1)*coordinates.upper-log_beta_positive(a,b)-width_log;
  int ea=0,eb=0,ex=0;
  std::frexp(a,&ea);std::frexp(b,&eb);
  const int shape_exponent=std::max(ea,eb);
  const double na=std::scalbn(a,-shape_exponent), nb=std::scalbn(b,-shape_exponent);
  std::frexp(std::max(std::abs(x),std::max(std::abs(lo),std::abs(hi))),&ex);
  const double nx=std::scalbn(x,-ex), nl=std::scalbn(lo,-ex), nh=std::scalbn(hi,-ex);
  const std::array<double,4> first{{na,nb,-nb,-na}}, second{{nx,nx,nl,nh}};
  std::array<double,9> expansion{};
  std::size_t length=0;
  for(std::size_t k=0;k<4;++k) {
    const double product=first[k]*second[k];
    const std::array<double,2> pieces{{product,std::fma(first[k],second[k],-product)}};
    for(double piece:pieces) {
      double carry=piece;
      for(std::size_t i=0;i<length;++i) {
        const double sum=carry+expansion[i];
        expansion[i]=addition_residual(carry,expansion[i],sum);carry=sum;
      }
      expansion[length++]=carry;
    }
  }
  double delta=0;
  for(std::size_t i=0;i<length;++i) delta+=expansion[i];
  delta/=(nh-nl);
  const double u=delta/na, v=-delta/nb;
  const double logsum=std::log(std::max(a,b))+std::log1p(std::min(a,b)/std::max(a,b));
  const double p=std::abs(u)<=0.5?scaled_positive_deviance(a,u)
      : a*(u-(coordinates.lower+logsum-std::log(a)));
  const double q=std::abs(v)<=0.5?scaled_positive_deviance(b,v)
      : b*(v-(coordinates.upper+logsum-std::log(b)));
  const double constant=0.5*(std::log(a)+std::log(b)-logsum)-0.91893853320467274178
      -density_stirling_remainder(a)-density_stirling_remainder(b)+density_stirling_remainder(a+b);
  return constant-coordinates.lower-coordinates.upper-width_log-p-q;
}

// Positive discrete series, scaled by its first term. ratio_bound(k) bounds
// every subsequent ratio; stop only when the geometric remainder is <= 2e-16
// of the accumulated sum. Used by Poisson, NB, binomial and truncated laws.
template <typename Ratio, typename Bound>
inline double log_series(double first, double k, Ratio ratio, Bound ratio_bound) {
  if (first == kNegInf) return first;
  double term=1.0, total=1.0;
  for (int i=0; i<1000000; ++i) {
    const double r=ratio(k), bound=ratio_bound(k);
    if (bound < 1.0 && term*bound <= 2e-16*total*(1.0-bound))
      return first+std::log(total);
    term*=r; total+=term;
    const double next=k+1.0;
    if (next==k) {
      // At enormous counts the ratios are constant to binary64 precision.
      return bound<1.0 ? first+std::log(total+term*r/(1.0-r)) : kNaN;
    }
    k=next;
  }
  return kNaN;
}
// The binary64 value of -log(2) lies strictly above the exact logarithm.
// Preserve that side of an exact half-probability jump after log rounding.
inline LogTails discrete_log_tails(double p,bool lower,bool log) {
  LogTails t=quantile_log_tails(p,lower,log);
  if(log && p==-0.69314718055994530942) {
    if(lower) t.upper=std::nextafter(t.upper,kNegInf);
    else t.lower=std::nextafter(t.lower,kNegInf);
  }
  return t;
}
// A reached predicate returns -1 on evaluation failure, 0 below the jump,
// and 1 at/above it. Bounds are integer-valued doubles. In exact arithmetic
// flooring a midpoint leaves width <= ceil(width/2). The largest finite
// binary64 bracket has width < 2^(max_exponent+1), so max_exponent+3
// halvings suffice to reach unit width or adjacent representable bounds.
// The checked invariant reached(lo)==0, reached(hi)==1 then certifies hi
// on the integer lattice up to 2^53 and the binary64 lattice beyond it.
template <typename Reached>
inline double discrete_bisect(double lo, double hi, Reached reached) {
  if (!std::isfinite(lo) || !std::isfinite(hi) || !(lo<hi)) return kNaN;
  if (reached(lo)!=0 || reached(hi)!=1) return kNaN;
  for (int i=0; i<std::numeric_limits<double>::max_exponent+3; ++i) {
    if (hi-lo<=1 || std::nextafter(lo,hi)==hi) return hi;
    const double width=hi-lo;
    const double mid=std::floor(std::isfinite(width) ? lo+width*.5 : lo*.5+hi*.5);
    if (!(mid>lo && mid<hi)) return kNaN;
    const int hit=reached(mid);
    if (hit<0) return kNaN;
    if (hit) hi=mid; else lo=mid;
  }
  return kNaN;
}
// Expand an unbounded nonnegative support at most max_exponent+1 times:
// hi starts >= 1 and doubles, with a final clamped evaluation at DBL_MAX.
template <typename Reached>
inline double discrete_search(double minimum, double maximum, Reached reached) {
  const int first=reached(minimum);
  if (first<0) return kNaN;
  if (first) return minimum;
  double lo=minimum, hi=std::isfinite(maximum) ? maximum : std::max(1.0,minimum+1.0);
  for (int i=0; i<std::numeric_limits<double>::max_exponent+1; ++i) {
    const int hit=reached(hi);
    if (hit<0) return kNaN;
    if (hit) return discrete_bisect(lo,hi,reached);
    if (std::isfinite(maximum)) return kNaN;
    if (hi==std::numeric_limits<double>::max())
      return reached(kPosInf)==1 ? kPosInf : kNaN;
    lo=hi;
    hi=hi>=std::numeric_limits<double>::max()/2 ? std::numeric_limits<double>::max() : hi*2;
  }
  return kNaN;
}
// Two-sided integer support. Expansion doubles a step >= 1, at most
// max_exponent+2 times in either direction, clamping before overflow.
template <typename Reached>
inline double discrete_signed_search(double centre, double step, Reached reached) {
  if (!std::isfinite(centre) || !std::isfinite(step) || step<1) return kNaN;
  const double largest=std::numeric_limits<double>::max();
  double lo=std::floor(centre), hi=lo, delta=step;
  bool bracketed=false;
  for (int i=0; i<std::numeric_limits<double>::max_exponent+2; ++i) {
    const int hit=reached(lo);
    if (hit<0) return kNaN;
    if (!hit) {bracketed=true;break;}
    if (lo==-largest)
      return reached(kNegInf)==0 ? lo : kNaN;
    hi=lo;
    const double next=std::floor(lo-delta);
    const double candidate=next>=lo ? std::nextafter(lo,-largest) :
                           std::isfinite(next) ? next : -largest;
    if (!(candidate<lo)) return kNaN;
    lo=candidate;
    delta=delta>=largest/2 ? largest : delta*2;
  }
  if (!bracketed) return kNaN;
  delta=step;bracketed=false;
  for (int i=0; i<std::numeric_limits<double>::max_exponent+2; ++i) {
    const int hit=reached(hi);
    if (hit<0) return kNaN;
    if (hit) {bracketed=true;break;}
    if (hi==largest)
      return reached(kPosInf)==1 ? kPosInf : kNaN;
    lo=hi;
    const double next=std::ceil(hi+delta);
    const double candidate=next<=hi ? std::nextafter(hi,largest) :
                           std::isfinite(next) ? next : largest;
    if (!(candidate>hi)) return kNaN;
    hi=candidate;
    delta=delta>=largest/2 ? largest : delta*2;
  }
  return bracketed ? discrete_bisect(lo,hi,reached) : kNaN;
}
// Left-continuity comparison shared by every discrete quantile. On the
// supplied scale, e=64*DBL_EPSILON: linear lower uses p*(1-e), upper
// uses p*(1+e) only if 1-p>4*e (R's guard against crossing one).
// Log lower uses p*(1+e), log upper p*(1-e); at -DBL_MAX leave the
// lower target finite instead of overflowing. Endpoints are handled by
// callers BEFORE fuzzing. This follows R's scale-dependent adjustment,
// with a uniform 64-epsilon factor (R 4.6 uses 8 linear / 2 log).
// A band larger than a jump may select earlier support points, however
// many the first adjusted crossing requires; never clamp it to one jump.
// Subnormal linear multiplication can round back to p; log inputs retain
// their range without exponentiation. NaN CDF evaluations remain failures.
inline int discrete_reached(double value, double p, bool lower, bool log, bool fuzz=true) {
  if (std::isnan(value) || std::isnan(p)) return -1;
  if (!fuzz) return int(lower ? value>=p : value<=p);
  constexpr double e=64*std::numeric_limits<double>::epsilon();
  double target=p;
  if (log) {
    if (lower) {
      if (p > -std::numeric_limits<double>::max()/(1+e)) target=p*(1+e);
    } else target=p*(1-e);
  } else if (lower) target=p*(1-e);
  else if (1-p>4*e) target=p*(1+e);
  return int(lower ? value>=target : value<=target);
}
// First adjusted crossing on the integer / representable-double lattice.
template <typename Tails>
inline double discrete_quantile(const LogTails &target, double minimum,
                                double maximum, Tails tails, double p=kNaN,
                                bool input_lower=true, bool input_log=true, bool fuzz=true) {
  if (!target.valid) return kNaN;
  if (target.lower==kNegInf) return minimum;
  if (target.upper==kNegInf) return maximum;
  const bool supplied=!std::isnan(p);
  const bool lower=supplied && fuzz ? input_lower : target.lower<target.upper;
  const bool log=supplied && fuzz ? input_log : true;
  const double goal=supplied && fuzz ? p : (lower ? target.lower : target.upper);
  return discrete_search(minimum,maximum,[&](double x) {
    const LogTails t=tails(x);
    if (!t.valid || std::isnan(t.lower) || std::isnan(t.upper)) return -1;
    // Samplers retain the original smaller-log-tail comparison below 2^53
    // and the supplied-scale comparison above it. Public quantiles use fuzz.
    if(!fuzz && supplied && std::abs(x)>9007199254740992.0)
      return discrete_reached(tail_probability(t,input_lower,input_log),p,input_lower,input_log,false);
    return discrete_reached(tail_probability(t,lower,log),goal,lower,log,fuzz);
  });
}

// Standard normal smaller log tail. Mills' continued fraction remains in
// range after erfc underflows; 80 levels converge to double precision at 8.
inline double normal_log_upper(double z) {
  if (std::isnan(z)) return kNaN;
  if (z < 0.0) return log1mexp(normal_log_upper(-z));
  if (z < 8.0) return std::log(std::erfc(z / std::sqrt(2.0)) / 2.0);
  if (z == kPosInf) return kNegInf;
  double r = 0.0;
  for (int k = 80; k >= 1; --k) r = k / (z + r);
  return -(0.5*z)*z - 0.91893853320467274178 - std::log(z+r);
}
// exp(log(p)) - 1/2 near the median, with the residual of binary64 log(2).
// This preserves the nonzero quantile at log(p) = double(-log(2)).
inline double probability_offset(double p, bool log_p) {
  return log_p ? 0.5 * std::expm1((p + 0.69314718055994530942) +
                                 2.3190468138462996155e-17) : p - 0.5;
}
inline double normal_quantile(double p, bool lower, bool log_p) {
  const auto t = quantile_log_tails(p, lower, log_p);
  if (!t.valid) return kNaN;
  if (t.lower == kNegInf) return kNegInf;
  if (t.upper == kNegInf) return kPosInf;
  const double offset = probability_offset(p, log_p) * (lower ? 1.0 : -1.0);
  if (std::abs(offset) < 1e-3) {
    const double y=offset*2.5066282746310005024, y2=y*y;
    return y*(1.0+y2*(1.0/6.0+y2*(7.0/120.0)));
  }
  const bool left = t.lower < t.upper;
  const double target = left ? t.lower : t.upper;
  double lo = 0.0, hi = std::sqrt(-target)*std::sqrt(2.0) + 1.0;
  for (int i = 0; i < 100; ++i) {
    const double mid = lo + (hi-lo)/2;
    if (mid == lo || mid == hi) break;
    if (normal_log_upper(mid) > target) lo = mid; else hi = mid;
  }
  const double z = lo + (hi-lo)/2;
  return left ? -z : z;
}
// Q(z)/phi(z) and 1-z*Q(z)/phi(z). The second quantity is also
// -M'(z), needed by normal convolutions and inverse-Gaussian tails.
inline double normal_log_mills(double z) {
  if (z < 8.0) return normal_log_upper(z)+(0.5*z)*z+0.91893853320467274178;
  double r=0.0;
  for (int k=80;k>=1;--k) r=k/(z+r);
  return -std::log(z+r);
}
inline double normal_log_mills_slope(double z) {
  if (z < 8.0) return std::log1p(-z*std::exp(normal_log_mills(z)));
  double r=0.0;
  for (int k=80;k>=2;--k) r=k/(z+r);
  const double h=1.0/(z+r);
  return std::log(h)-std::log(z+h);
}
// location + sign(z)*scale*expm1(abs(z)), retaining a finite affine
// result even when the distance overflows. Used by slash and Bhattacharjee.
inline double expm1_affine(double z, double location, double scale) {
  const double t=std::abs(z), sign=z<0 ? -1.0 : 1.0;
  if (t<700.0) return std::fma(sign*scale,std::expm1(t),location);
  const double ld=std::log(scale)+t+log1mexp(-t);
  const double half=std::exp(ld-std::log(2.0));
  return std::fma(sign*2.0,half,location);
}

// Gamma helpers require special.hpp for these no-throw backend wrappers.
inline double gamma_p(double a, double x);
inline double gamma_q(double a, double x);
inline double lgamma1p(double x);
// Unit-scale gamma evaluated in log coordinates, shared by the gamma,
// inverse gamma, loggamma and transformed gamma families.
inline LogTails gamma_log_tails(double lx, double a, double supplied_x=kNaN) {
  if (!(a > 0.0 && std::isfinite(a)) || std::isnan(lx))
    return {kNaN, kNaN, false};
  if (lx == kNegInf) return tails_from_log(kNegInf, true);
  if (lx > std::log(std::numeric_limits<double>::max()))
    return tails_from_log(kNegInf, false);
  const double x = std::isnan(supplied_x)?std::exp(lx):supplied_x;
  if(a>=16 && x>0 && std::isfinite(x)) {
    const double root=std::sqrt(a), w=root*log_positive_ratio(x,a);
    return tails_from_log(gamma_centered::log_tail(w,1/root),w<0);
  }
  if (lx < -36.0)
    return tails_from_log(a*lx - (a<=1.0/32?lgamma1p(a):std::lgamma(a+1.0)), true);
  const double p = gamma_p(a,x), q = gamma_q(a,x);
  if (p > 1e-280 && p <= 0.5) return tails_from_log(std::log(p), true);
  if (q > 1e-280 && q < 0.5) return tails_from_log(std::log(q), false);
  if (x < a+1.0) {
    double sum = 1.0, term = 1.0;
    bool converged=false;
    for (int k=1; k<100000; ++k) {
      term *= x/(a+k); sum += term;
      if (term <= sum*2e-16) {converged=true;break;}
    }
    if(!converged) return {kNaN,kNaN,false};
    return tails_from_log(gamma_log_density_positive(x,a,1)+lx-std::log(a)+std::log(sum), true);
  }
  // Lentz fraction for Q(a,x); only used outside the backend's safe range.
  const double tiny = 1e-300;
  double b=x+1.0-a, c=1.0/tiny, d=1.0/b, h=d;
  bool converged=false;
  for (int k=1; k<100000; ++k) {
    const double an=-static_cast<double>(k)*(k-a);
    b += 2.0; d=an*d+b; if (std::abs(d)<tiny) d=tiny;
    c=b+an/c; if (std::abs(c)<tiny) c=tiny;
    d=1.0/d; const double delta=d*c; h*=delta;
    if (std::abs(delta-1.0)<4e-16) {converged=true;break;}
  }
  if(!converged || !(h>0)) return {kNaN,kNaN,false};
  return tails_from_log(gamma_log_density_positive(x,a,1)+lx+std::log(h), false);
}
inline LogTails gamma_centered_physical_tails(double x,double a,double scale) {
  int es=0,ea=0;
  const double ms=std::frexp(scale,&es),ma=std::frexp(a,&ea);
  const double mean=ms*ma, low=std::fma(ms,ma,-mean);
  const double normalized=std::scalbn(x,-(es+ea));
  double lr;
  if(normalized>=mean*0.5 && normalized<=mean*2)
    lr=std::log1p(((normalized-mean)-low)/mean);
  else if(normalized>=std::numeric_limits<double>::min() && std::isfinite(normalized))
    lr=std::log(normalized)-std::log(mean);
  else lr=std::log(x)-std::log(scale)-std::log(a);
  const double root=std::sqrt(a), w=root*lr;
  return tails_from_log(gamma_centered::log_tail(w,1/root),w<0);
}
inline bool gamma_laplace_tails(double x,double a,double scale,LogTails &answer) {
  if(a<1 || !(x>0) || !std::isfinite(x)) return false;
  int es=0,ea=0;
  const double ms=std::frexp(scale,&es), ma=std::frexp(a,&ea);
  const double mean=ms*ma, low=std::fma(ms,ma,-mean);
  const double normalized=std::scalbn(x,-(es+ea));
  if(!std::isfinite(normalized)) return false;
  const double one_scale=std::scalbn(ms,-ea);
  const double difference=((normalized-mean)-low)+one_scale;
  if(difference==0) return false;
  const double log_units=std::log(std::abs(difference))-std::log(one_scale);
  // Upper remainder <= H/h^2. For the lower tail, restricting to [x/2,x]
  // gives <=4H/h^2 plus exp(-h*x/2); this test bounds both by 1e-12.
  if(std::log(4.0)+std::log(a-1)-2*log_units>=std::log(5e-13)) return false;
  const double log_slope=std::log(std::abs(difference))+(es+ea)*0.69314718055994530942
      -std::log(x)-std::log(scale);
  const double lp=gamma_log_density_positive(x,a,scale)-log_slope;
  answer=tails_from_log(lp,difference<0);
  return true;
}
inline bool gamma_mean_quantile_enclosed(double a,const LogTails &tails) {
  const double epsilon=1e-10;
  return tails.valid && a*(epsilon*epsilon/(4*(1+epsilon))) > -std::min(tails.lower,tails.upper);
}
inline double gamma_log_quantile(double p, double a, bool lower, bool log_p) {
  const auto t = quantile_log_tails(p,lower,log_p);
  if (!t.valid || !(a>0.0 && std::isfinite(a))) return kNaN;
  if (t.lower == kNegInf) return kNegInf;
  if (t.upper == kNegInf) return kPosInf;
  const bool left=t.lower<t.upper;
  const double target=left?t.lower:t.upper;
  const double leading=(t.lower+(a<=1.0/32?lgamma1p(a):std::lgamma(a+1.0)))/a;
  if (leading < -36.0) return leading;
  if(!std::isfinite(leading)) return kNaN;
  double lo=std::min(-1.0,leading-2.0), hi=std::max(1.0,std::log(a)+1.0);
  bool valid=true;
  auto before=[&](double z) {
    const auto v=gamma_log_tails(z,a);
    if(!v.valid || std::isnan(v.lower) || std::isnan(v.upper)) {valid=false;return false;}
    return left ? v.lower<target : v.upper>target;
  };
  while (before(hi)) {if(!valid || !std::isfinite(hi)) return kNaN;hi=hi*2.0+1.0;}
  if(!valid) return kNaN;
  while (!before(lo)) {if(!valid || !std::isfinite(lo)) return kNaN;lo=lo*2.0-1.0;}
  if(!valid || !std::isfinite(lo) || !std::isfinite(hi)) return kNaN;
  for (int i=0; i<150; ++i) {
    const double mid=lo+(hi-lo)/2;
    if (mid==lo || mid==hi) break;
    if (before(mid)) lo=mid; else hi=mid;
    if(!valid) return kNaN;
  }
  return hi-lo<=1e-10?lo+(hi-lo)/2:kNaN;
}

} // namespace special
} // namespace cppunivariate
