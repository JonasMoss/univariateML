// Tail-accurate d/p/q rules: compute the smaller tail directly in log space;
// derive its complement with log1mexp, never 1-p near p=1. Quantiles use
// the accurate log tail, including log inputs -1e4 and below; do not first
// exponentiate them. Handle p=0/1 (log p=-Inf/0) at support endpoints.
// Preserve logarithms of underflowed intermediates analytically when needed.
// Double only: guard range with log-space algebra; CPPUNIV_EXTENDED_REAL equals
// double on some CRAN platforms. Extended precision may improve rounding,
// but must never be required to keep an intermediate in range.
// Example: Lomax log_upper=-kappa*log1p_product(lambda,x);
// q=expm1(-log_upper/kappa)/lambda (guard intermediate overflow).
#pragma once
#include "../core/precision.hpp"
#include "constants.hpp"
#include <cmath>
#include <algorithm>

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
  const double product = a*b;
  return product == kPosInf ? std::log(a)+std::log(b)
                           : log_softplus(product);
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
  while(z<16.0 || z+h<16.0) {
    correction+=std::log(z)-std::log(z+h); z+=1.0;
  }
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
// Integer inversion uses the accurate smaller log tail throughout. Stops at
// adjacent representable values once integers cease to be representable.
template <typename Tails>
inline double discrete_quantile(const LogTails &target, double minimum,
                                double maximum, Tails tails) {
  if (!target.valid) return kNaN;
  if (target.lower==kNegInf) return minimum;
  if (target.upper==kNegInf) return maximum;
  const bool lower=target.lower < target.upper;
  const auto below=[&](double x) {
    const LogTails t=tails(x);
    return lower ? t.lower<target.lower : t.upper>target.upper;
  };
  if (!below(minimum)) return minimum;
  double lo=minimum, hi=std::isfinite(maximum) ? maximum : std::max(1.0,minimum+1.0);
  while (below(hi)) {
    lo=hi;
    if (hi>=std::numeric_limits<double>::max()/2.0) {
      hi=std::numeric_limits<double>::max();
      if (below(hi)) return kPosInf;
      break;
    }
    hi*=2.0;
  }
  while (hi-lo>1.0) {
    const double mid=std::floor(std::isfinite(hi-lo)?lo+(hi-lo)*0.5:lo*0.5+hi*0.5);
    if (mid==lo || mid==hi) break;
    if (below(mid)) lo=mid; else hi=mid;
  }
  return hi;
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
// Unit-scale gamma evaluated in log coordinates, shared by the gamma,
// inverse gamma, loggamma and transformed gamma families.
inline LogTails gamma_log_tails(double lx, double a) {
  if (!(a > 0.0 && std::isfinite(a)) || std::isnan(lx))
    return {kNaN, kNaN, false};
  if (lx == kNegInf) return tails_from_log(kNegInf, true);
  if (lx > std::log(std::numeric_limits<double>::max()))
    return tails_from_log(kNegInf, false);
  const double x = std::exp(lx);
  if (lx < -36.0)
    return tails_from_log(a*lx - std::lgamma(a+1.0), true);
  const double p = gamma_p(a,x), q = gamma_q(a,x);
  if (p > 1e-280 && p <= 0.5) return tails_from_log(std::log(p), true);
  if (q > 1e-280 && q < 0.5) return tails_from_log(std::log(q), false);
  if (x < a+1.0) {
    double sum = 1.0, term = 1.0;
    for (int k=1; k<100000; ++k) {
      term *= x/(a+k); sum += term;
      if (term <= sum*2e-16) break;
    }
    return tails_from_log(a*lx-x-std::lgamma(a+1.0)+std::log(sum), true);
  }
  // Lentz fraction for Q(a,x); only used outside the backend's safe range.
  const double tiny = 1e-300;
  double b=x+1.0-a, c=1.0/tiny, d=1.0/b, h=d;
  for (int k=1; k<100000; ++k) {
    const double an=-static_cast<double>(k)*(k-a);
    b += 2.0; d=an*d+b; if (std::abs(d)<tiny) d=tiny;
    c=b+an/c; if (std::abs(c)<tiny) c=tiny;
    d=1.0/d; const double delta=d*c; h*=delta;
    if (std::abs(delta-1.0)<4e-16) break;
  }
  return tails_from_log(a*lx-x-std::lgamma(a)+std::log(h), false);
}
inline double gamma_log_quantile(double p, double a, bool lower, bool log_p) {
  const auto t = quantile_log_tails(p,lower,log_p);
  if (!t.valid || !(a>0.0 && std::isfinite(a))) return kNaN;
  if (t.lower == kNegInf) return kNegInf;
  if (t.upper == kNegInf) return kPosInf;
  const bool left=t.lower<t.upper;
  const double target=left?t.lower:t.upper;
  const double leading=(t.lower+std::lgamma(a+1.0))/a;
  if (leading < -36.0) return leading;
  double lo=std::min(-1.0,leading-2.0), hi=std::max(1.0,std::log(a)+1.0);
  auto before=[&](double z) {
    const auto v=gamma_log_tails(z,a);
    return left ? v.lower<target : v.upper>target;
  };
  while (before(hi)) hi=hi*2.0+1.0;
  while (!before(lo)) lo=lo*2.0-1.0;
  for (int i=0; i<150; ++i) {
    const double mid=lo+(hi-lo)/2;
    if (mid==lo || mid==hi) break;
    if (before(mid)) lo=mid; else hi=mid;
  }
  return lo+(hi-lo)/2;
}

} // namespace special
} // namespace cppunivariate
