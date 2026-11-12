// Ordinary PGW: H(x) = (1 + (x/scale)^shape)^exponent - 1.
#pragma once
#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {
inline bool pgw_parameters(double scale, double shape, double exponent) {
  return scale>0 && std::isfinite(scale) && shape>0 && std::isfinite(shape) &&
         exponent>0 && std::isfinite(exponent);
}
// Preserve close displacements, and avoid cancellation of large logs for
// moderate, normal ratios. Subnormal quotients keep their logarithms instead.
inline double pgw_log_ratio(double x, double scale) {
  const double ratio=x/scale;
  if (ratio>=0.5 && ratio<=2) return std::log1p((x-scale)/scale);
  return ratio>=std::numeric_limits<double>::min() && std::isfinite(ratio)
      ? std::log(ratio) : std::log(x)-std::log(scale);
}
// log(v), v=c*log1p(exp(b*log(x/a))). Retain underflowed powers.
inline double pgw_log_v(double x, double scale, double shape, double exponent) {
  return std::log(exponent)+special::log_softplus_product(shape,pgw_log_ratio(x,scale));
}
struct PgwRatioError { double hi; double lo; };
// Relative correction from a rounded normalized quotient q to mx/ma.
// Normalized operands keep every residual far from underflow. Two products
// and a corrected division retain the low bits before multiplication by b.
inline PgwRatioError pgw_ratio_error(double mx, double ma, double q) {
  const double product=q*ma, product_low=std::fma(q,ma,-product);
  const double difference=mx-product;
  const double residual=difference-product_low;
  const double low=special::addition_residual(difference,-product_low,residual);
  const double hi=residual/product;
  const double lo=(std::fma(-hi,product,residual)+low-hi*product_low)/product;
  return {hi,lo};
}
inline double pgw_v(double x, double scale, double lr, double shape,
                    double exponent, double lv) {
  // Guard before multiplying: avoid GCC's inference from an Inf product
  // after inlining finite-parameter checks (see log_softplus_product).
  if (lr>1 && shape>=std::numeric_limits<double>::max()/lr)
    return (exponent*shape)*lr;
  const double z=shape*lr;
  if (z< -36) {
    // log1p(t)/t differs from one by <1.2e-16 here. Form c*t directly
    // without rounding an underflowed t or subtracting log(c) and log(t).
    const int ex=std::ilogb(x)+1, ea=std::ilogb(scale)+1;
    const double mx=std::scalbn(x,-ex), ma=std::scalbn(scale,-ea);
    if (shape<=2) {
      const int ec=std::ilogb(exponent)+1;
      const double mc=std::scalbn(exponent,-ec), q=mx/ma;
      const PgwRatioError error=pgw_ratio_error(mx,ma,q);
      const double base=mc*std::pow(q,shape)*
          std::exp2(std::fma(shape,static_cast<double>(ex-ea),static_cast<double>(ec)));
      if (std::isfinite(base))
        return std::fma(base,std::expm1(shape*std::log1p(error.hi)),base);
    } else {
      const double ratio=x/scale;
      if (ratio>=std::numeric_limits<double>::min() && ratio<1) {
        // A half power remains normal when c can rescue t into a
        // significant hazard. Correct its rounded input before squaring.
        const double half=shape*0.5, power=std::pow(ratio,half);
        if (power>=std::numeric_limits<double>::min() && std::isfinite(power)) {
          const double q=std::scalbn(ratio,ea-ex);
          const PgwRatioError error=pgw_ratio_error(mx,ma,q);
          const double high=half*error.hi;
          const double low=std::fma(half,error.hi,-high)+
              half*(error.lo-0.5*error.hi*error.hi);
          const double leading=power*std::exp(high);
          const double corrected=std::fma(leading,std::expm1(low),leading);
          if (corrected>0 && std::isfinite(corrected)) return (exponent*corrected)*corrected;
        }
      }
    }
  }
  const double L=special::softplus(z), v=exponent*L;
  // Rescaling a subnormal L would retain its initial rounding error.
  return L>=std::numeric_limits<double>::min() && v>0 && std::isfinite(v)
      ? v : std::exp(lv);
}
inline double dpgw(double x, double scale, double shape, double exponent, bool log) {
  if (!pgw_parameters(scale,shape,exponent) || std::isnan(x)) return special::kNaN;
  if (x<0 || x==special::kPosInf) return log ? special::kNegInf : 0;
  if (x==0) {
    if (shape<1) return special::kPosInf;
    if (shape>1) return log ? special::kNegInf : 0;
    return log ? std::log(exponent)-std::log(scale) : exponent/scale;
  }
  const double lr=pgw_log_ratio(x,scale), z=shape*lr;
  const double lv=pgw_log_v(x,scale,shape,exponent), v=pgw_v(x,scale,lr,shape,exponent,lv);
  // z-log1p(exp(z)) = -softplus(-z); avoids Inf-Inf for large z.
  const double h=std::expm1(v);
  const double ld=std::isinf(h) ? special::kNegInf :
      std::log(shape)+std::log(exponent)-std::log(x)-special::softplus(-z)+v-h;
  return log ? ld : std::exp(ld);
}
inline double ppgw(double q, double scale, double shape, double exponent,
                   bool lower_tail, bool log_p) {
  if (!pgw_parameters(scale,shape,exponent) || std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  if (q==special::kPosInf) return special::tail_probability(special::tails_from_log(special::kNegInf,false),lower_tail,log_p);
  const double lv=pgw_log_v(q,scale,shape,exponent);
  const double v=pgw_v(q,scale,pgw_log_ratio(q,scale),shape,exponent,lv);
  const double lh=lv < -36 ? lv : std::log(std::expm1(v));
  return special::tail_probability(special::tails_from_log_hazard(lh,false),lower_tail,log_p);
}
inline double qpgw(double p, double scale, double shape, double exponent,
                   bool lower_tail, bool log_p) {
  if (!pgw_parameters(scale,shape,exponent)) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower==special::kNegInf) return 0;
  if (t.upper==special::kNegInf) return special::kPosInf;
  // H(a)=2^c-1 is exactly representable for integer c<=53. Preserve this
  // algebraic center even when a tiny b makes every rounding bit matter.
  if (!lower_tail && log_p && exponent<=53 && exponent==std::floor(exponent) &&
      p==1-std::ldexp(1.0,static_cast<int>(exponent))) return scale;
  // w=log1p(H), v=w/c, log(t_power)=log(expm1(v)).
  const double lh=t.lower < -0.6931471805599453 ?
      special::log_neg_log1mexp(t.lower) : std::log(-t.upper);
  const double w=std::log1p(-t.upper);
  const double lv=special::log_softplus(lh)-std::log(exponent);
  double v=w/exponent;
  if (w<std::numeric_limits<double>::min() && log_p && lower_tail) {
    // H and log1p(H) equal F up to relative O(F^2) here. Form F/c
    // from the original log input and c's exact binary decomposition.
    // FMA plus the low part of log(2) avoids cancellation of two large logs.
    const int binary_exponent=std::ilogb(exponent)+1;
    const double mantissa=std::scalbn(exponent,-binary_exponent);
    const double reduced=std::fma(-static_cast<double>(binary_exponent),
        0.69314718055994530942,t.lower)
        -binary_exponent*2.3190468138462995584e-17;
    v=std::exp(reduced)/mantissa;
  }
  if (lv>std::log(36.0)) {
    const double coordinate=std::isfinite(v) ? v/shape : std::exp(lv-std::log(shape));
    return special::shifted_scaled_exp(0,scale,coordinate);
  }
  const double lt=lv < -36 ? lv : std::log(std::expm1(v));
  // Near t_power=1, log(expm1(v)) subtracts nearly equal quantities.
  // Binary64 cannot resolve that displacement when a vanishing b amplifies
  // its rounding into a finite quantile; report the documented narrow region.
  if (std::abs(lt)<0.2 && shape<1e-4) {
    // A practical 1e-12 absolute margin in this bounded log-power region
    // is conservative for the log1p/expm1 composition. Return a rounded
    // endpoint only when both sides of that margin imply the same endpoint.
    const double la=std::log(scale), margin=1e-12;
    if (lt>margin && la+(lt-margin)/shape>std::log(std::numeric_limits<double>::max())+1)
      return special::kPosInf;
    if (lt< -margin && la+(lt+margin)/shape<std::log(std::numeric_limits<double>::denorm_min())-1)
      return 0;
    return special::kNaN;
  }
  return special::shifted_scaled_power_quantile(0,scale,lt,shape);
}
template <typename Rng>
inline double rpgw(Rng &rng, double scale, double shape, double exponent) {
  return qpgw(rng.runif(0.0,1.0),scale,shape,exponent,true,false);
}
} // namespace distributions
} // namespace cppunivariate
