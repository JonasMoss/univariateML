// Truncated normal d/p/q/r on [a, b]; finite bounds use the inside density limit.
// Use normal log-tail ratios, with local quadrature for narrow intervals
// and parent parameters whose standardization loses endpoint offsets.
#pragma once
#include "mle.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <utility>

namespace cppunivariate {
namespace distributions {

inline bool tnorm_valid(double mean, double sd, double a, double b) {
  return std::isfinite(mean) && std::isfinite(sd) && sd > 0 && !std::isnan(a) &&
         !std::isnan(b) && a < b && a < std::numeric_limits<double>::infinity() &&
         b > -std::numeric_limits<double>::infinity();
}

// log(Phi(beta) - Phi(alpha)), alpha < beta.
inline double tnorm_log_mass(double alpha, double beta) {
  if (!(alpha < beta)) return -std::numeric_limits<double>::infinity();
  // Use the survival function in the upper tail and CDF in the lower
  // tail. Very narrow intervals and underflow use shifted quadrature.
  const bool upper = alpha >= 0;
  const double big = upper ? special::normal_log_upper(alpha)
                           : special::normal_log_upper(-beta);
  const double small = upper ? special::normal_log_upper(beta)
                             : special::normal_log_upper(-alpha);
  if (big == special::kNegInf) return big;
  const double delta = small - big;
  if (std::isfinite(big) && delta < -1e-4)
    return big + std::log(-std::expm1(delta));
  return tnorm_detail::mass(0, -0.5, alpha, beta, false).log_mass - 0.91893853320467274178;
}

// log(1 - exp(v)), v <= 0.
inline double tnorm_log1mexp(double v) {
  return special::log1mexp(v);
}

// Coordinates about the interval mode preserve offsets when parent parameters
// diverge near an exponential face. Use the interval width for narrow support.
struct TnormCoordinates {
  double centre, scale, t1, t2, lo, hi;
};
inline TnormCoordinates tnorm_coordinates(double mean, double sd, double a, double b) {
  const double c = std::min(std::max(mean, a), b);
  const double s = std::min(sd, b - a);
  return {c, s, ((mean - c) / sd) * (s / sd), -0.5 * (s / sd) * (s / sd),
          (a - c) / s, (b - c) / s};
}

// The first two relative mass corrections are bounded by slope*d and
// curvature*d^2 in local coordinates. This also admits a zero slope
// without losing the quadratic guard.
inline bool tnorm_linear_endpoint(double log_distance, double log_scale,
                                   double slope, double curvature) {
  const double z=log_distance-log_scale;
  return z+std::log(std::abs(slope))<-36.0 &&
         2.0*z+std::log(std::abs(curvature))<-36.0;
}

inline double dtnorm(double x, double mean, double sd, double a, double b, bool log) {
  if (!tnorm_valid(mean, sd, a, b) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  if (!std::isfinite(x) || x < a || x > b) return log ? -std::numeric_limits<double>::infinity() : 0.0;
  const auto c = tnorm_coordinates(mean, sd, a, b);
  const double u = (x - c.centre) / c.scale;
  const double ld = u * (c.t1 + c.t2 * u) - std::log(c.scale) -
                    tnorm_detail::mass(c.t1, c.t2, c.lo, c.hi, false).log_mass;
  return log ? ld : std::exp(ld);
}

inline double ptnorm(double q, double mean, double sd, double a, double b,
                     bool lower_tail, bool log_p) {
  if (!tnorm_valid(mean, sd, a, b) || std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
  double below, above;  // log P(X <= q), log P(X > q)
  if (q <= a) {
    below = -std::numeric_limits<double>::infinity();
    above = 0;
  } else if (q >= b) {
    below = 0;
    above = -std::numeric_limits<double>::infinity();
  } else {
    // Endpoint masses can be representable on the log scale even when
    // standardizing their width underflows. The linear mass approximation
    // has relative correction below 2e-16 under the slope guard.
    const auto edge = tnorm_coordinates(mean,sd,a,b);
    for (bool low : {true,false}) {
      const double endpoint=low?a:b;
      if (!std::isfinite(endpoint)) continue;
      const double distance=low?q-a:b-q, ld=std::log(distance);
      const double u=low?edge.lo:edge.hi;
      const double slope=edge.t1+2.0*edge.t2*u;
      if (!tnorm_linear_endpoint(ld,std::log(edge.scale),slope,edge.t2)) continue;
      const double total=tnorm_detail::mass(edge.t1,edge.t2,edge.lo,edge.hi,false).log_mass;
      const double lp=ld+u*(edge.t1+edge.t2*u)-std::log(edge.scale)-total;
      return special::tail_probability(special::tails_from_log(lp,low),lower_tail,log_p);
    }
    const double alpha=special::scaled_difference(a,mean,sd), beta=special::scaled_difference(b,mean,sd);
    const double z=special::scaled_difference(q,mean,sd);
    if (beta-alpha>1e-4 && z>alpha && z<beta &&
        (!std::isfinite(a) || !std::isfinite(b) ||
         (std::max(std::abs(alpha),std::abs(beta))<100 &&
          std::abs(mean)<4*std::max(std::abs(a),std::abs(b))))) {
      const double total=tnorm_log_mass(alpha,beta);
      below=tnorm_log_mass(alpha,z)-total;
      above=tnorm_log_mass(z,beta)-total;
      const auto t=below<above ? special::tails_from_log(below,true) : special::tails_from_log(above,false);
      return special::tail_probability(t,lower_tail,log_p);
    }
    auto c = tnorm_coordinates(mean, sd, a, b);
    if (std::abs((q - c.centre) / c.scale) < 4) {
      c.centre = q;
      c.t1 = ((mean - q) / sd) * (c.scale / sd);
      c.lo = (a - q) / c.scale;
      c.hi = (b - q) / c.scale;
    }
    const double u = (q - c.centre) / c.scale;
    const double total = tnorm_detail::mass(c.t1, c.t2, c.lo, c.hi, false).log_mass;
    below = tnorm_detail::mass(c.t1, c.t2, c.lo, u, false).log_mass - total;
    above = tnorm_detail::mass(c.t1, c.t2, u, c.hi, false).log_mass - total;
    // The smaller probability is accurate; take the larger as its complement.
    if (below > above) below = tnorm_log1mexp(above);
    else above = tnorm_log1mexp(below);
  }
  const double lp = lower_tail ? below : above;
  return log_p ? lp : std::exp(lp);
}

inline double qtnorm(double p, double mean, double sd, double a, double b,
                     bool lower_tail, bool log_p) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (!tnorm_valid(mean, sd, a, b) || std::isnan(p) || (log_p ? p > 0 : (p < 0 || p > 1)))
    return nan;
  // Log probabilities below and above the quantile.
  double below = log_p ? p : std::log(p);
  double above = log_p ? tnorm_log1mexp(p) : std::log1p(-p);
  if (!lower_tail) std::swap(below, above);
  if (below == -std::numeric_limits<double>::infinity()) return a;
  if (above == -std::numeric_limits<double>::infinity()) return b;
  const double offset=special::probability_offset(p,log_p)*(lower_tail?1.0:-1.0);
  if (a-mean == -(b-mean) && std::abs(offset)<1e-4) {
    const double displacement=offset/std::exp(dtnorm(mean,mean,sd,a,b,true));
    const double r=displacement/sd;
    return std::fma(displacement,1+r*r/6,mean);
  }
  const bool near_lower=below<above;
  const double endpoint=near_lower?a:b;
  if (std::isfinite(endpoint)) {
    const auto edge=tnorm_coordinates(mean,sd,a,b);
    const double u=near_lower?edge.lo:edge.hi;
    const double slope=edge.t1+2.0*edge.t2*u;
    const double total=tnorm_detail::mass(edge.t1,edge.t2,edge.lo,edge.hi,false).log_mass;
    const double ld=u*(edge.t1+edge.t2*u)-std::log(edge.scale)-total;
    const double log_distance=(near_lower?below:above)-ld;
    if (tnorm_linear_endpoint(log_distance,std::log(edge.scale),slope,edge.t2))
      return endpoint+(near_lower?1.0:-1.0)*std::exp(log_distance);
  }
  const double alpha0=special::scaled_difference(a,mean,sd), beta0=special::scaled_difference(b,mean,sd);
  if (beta0-alpha0>1e-4 &&
      (!std::isfinite(a) || !std::isfinite(b) ||
       ((alpha0>4 || beta0< -4) &&
        std::max(std::abs(alpha0),std::abs(beta0))<100 &&
        std::abs(mean)<4*std::max(std::abs(a),std::abs(b))))) {
    const double mass=tnorm_log_mass(alpha0,beta0);
    const double l=special::log_sum_exp(special::normal_log_upper(-alpha0),below+mass);
    const double u=special::log_sum_exp(special::normal_log_upper(beta0),above+mass);
    const double z=l<u ? special::normal_quantile(l,true,true) : special::normal_quantile(u,false,true);
    return std::min(std::max(std::fma(sd,z,mean),a),b);
  }
  auto c = tnorm_coordinates(mean, sd, a, b);
  // Anchor a small tail at its finite endpoint, preserving tiny distances
  // instead of subtracting nearly equal mode-centred coordinates.
  const double anchor = below <= above ? a : b;
  if (std::isfinite(anchor) && std::abs((anchor - c.centre) / c.scale) < 4) {
    c.centre = anchor;
    c.t1 = ((mean - anchor) / sd) * (c.scale / sd);
    c.lo = (a - anchor) / c.scale;
    c.hi = (b - anchor) / c.scale;
  }
  const double alpha = c.lo, beta = c.hi;
  auto lm = [&](double lo, double hi) {
    if (!(lo < hi)) return -std::numeric_limits<double>::infinity();
    return tnorm_detail::mass(c.t1, c.t2, lo, hi, false).log_mass;
  };
  const double total = lm(alpha, beta);
  // Solve on the side with the smaller probability: h(z) = log mass on
  // that side minus its target, increasing in z for the lower side.
  const bool use_below = below <= above;
  const double target = use_below ? below : above;
  auto h = [&](double z) {
    return use_below ? lm(alpha, z) - total - target
                     : target - (lm(z, beta) - total);
  };
  // Bracket [lo, hi] with h(lo) < 0 < h(hi), expanding infinite ends.
  const double centre = std::min(std::max(0.0, alpha), beta);
  double lo = alpha, hi = beta;
  if (!std::isfinite(lo)) {
    double step = 1;
    lo = std::min(centre, hi) - step;
    while (h(lo) >= 0 && std::isfinite(lo)) { step *= 2; lo = std::min(centre, hi) - step; }
  }
  if (!std::isfinite(hi)) {
    double step = 1;
    hi = std::max(centre, lo) + step;
    while (h(hi) <= 0 && std::isfinite(hi)) { step *= 2; hi = std::max(centre, lo) + step; }
  }
  double z = lo + (hi - lo) / 2;
  for (int it = 0; it < 300; ++it) {
    const double value = h(z);
    if (value == 0) break;
    if (value < 0) lo = z; else hi = z;
    // Newton step with derivative phi(z) / (mass on the solved side).
    const double side = use_below ? lm(alpha, z) : lm(z, beta);
    const double slope = std::exp(z * (c.t1 + c.t2 * z) - side);
    double next = z - value / slope;
    if (!(next > lo && next < hi)) next = lo + (hi - lo) / 2;
    if (std::abs(next - z) <= std::numeric_limits<double>::epsilon() * std::max(1e-100, std::abs(z)) ||
        hi - lo <= std::numeric_limits<double>::epsilon() * std::max(1e-100, std::abs(z))) {
      z = next;
      break;
    }
    z = next;
  }
  const double x = c.centre + c.scale * z;
  return std::min(std::max(x, a), b);
}

template <typename Rng>
inline double rtnorm(Rng &rng, double mean, double sd, double a, double b) {
  if (!tnorm_valid(mean, sd, a, b)) return std::numeric_limits<double>::quiet_NaN();
  return qtnorm(rng.runif(0.0, 1.0), mean, sd, a, b, true, false);
}

}  // namespace distributions
}  // namespace cppunivariate
