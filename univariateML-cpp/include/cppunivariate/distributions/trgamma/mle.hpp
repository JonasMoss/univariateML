// Transformed gamma (Stacy's generalized gamma) MLE, actuar parameterisation
//   f(x) = shape2 * u^shape1 * exp(-u) / (x * Gamma(shape1)),
//   u = (x / scale)^shape2.
//
// For fixed shape2 = tau, z = x^tau is gamma(shape1, scale^tau): shape1
// solves log(a) - digamma(a) = K and the scale is closed form. Write
// y = log(x), u = (y - mean(y)) / sd(y) (divisor n), t = tau * sd(y) and
// K(t) = log(mean(exp(t * u))). The profile log likelihood per observation,
// relative to the lognormal fit, is
//   Delta(t) = log(t) + log(a)/2 - stirling(a) - a*K(t) + 1/2,
//   a = a(K(t)),  dDelta/dt = 1/t - a*K'(t),
// where stirling(a) = lgamma(a) - (a - 1/2) log(a) + a - log(2 pi)/2.
// Delta -> 0 as t -> 0 (lognormal face, slope -mean(u^3)/6 there) and
// Delta -> log(2 pi)/2 - 1/2 - log(max u) as t -> inf (power face with
// endpoint max(x)), from below. The free fit scans t on a log grid,
// refines every sign change of the slope and compares with both faces;
// positive finite improvements within 1e-8 per observation stop as unresolved
// comparisons. A uniform seam bound and root
// inclusion close the profile certificate. Fixed shape1 gives a strictly
// concave problem in log(t); fixed shape2 is closed form.
//
// Reuse (e.g. by a transformed beta fitter): for a completed certificate,
// logLik is the supremum over the family and its two faces; `status`
// says where it is attained. trgamma_fit_log() takes log data, so a
// reciprocal fit passes -log(x) without forming 1/x.

#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/precision.hpp"
#include "../burr/mle.hpp"
#include <array>

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class TrGammaStatus {
  Ok,
  LognormalBoundary,
  PowerBoundary,
  ScaleOutOfRange,
  InvalidData,
  InvalidFixed,
  IterationFailure
};

struct TrGammaResult {
  double shape1, shape2, scale;
  double log_scale;
  double logLik;
  int iterations;
  TrGammaStatus status;
 StopDetail stop_detail{};
 bool reconstruction_failure=false;
 bool transformation_failure=false; };

namespace trgamma_detail {

constexpr double kHalfLog2Pi = 0.91893853320467274178;
constexpr double kEulerGamma = 0.57721566490153286061;
constexpr double kTiePerObs = 1e-8;
// Below t = kMinT the shape exceeds 1e200 and no fit is representable.
constexpr double kMinT = 1e-100;

inline bool valid_fit_request(double tol,int maxit,double a,double b) {
  if(!(tol>0) || !std::isfinite(tol) || maxit<1)return false;
  for(double v:{a,b})if(!std::isnan(v) && (!(v>0)||!std::isfinite(v)))return false;
  return true;
}
// Intentional raw-data ties are allowed. A new tie introduced by logarithms
// changes multiplicities and endpoint traces, so it cannot enter a certificate.
inline bool lost_log_distinctions(span x,span logs,double endpoint=NAN,double logendpoint=NAN) {
  struct Observation {double x,y;};
  std::vector<Observation> sorted;
  for(std::size_t i=0;i<x.size();++i) {
    if(!(x[i]>0) || !std::isfinite(x[i]) || !std::isfinite(logs[i]))return false;
    if(!std::isnan(endpoint) && x[i]!=endpoint && logs[i]==logendpoint)return true;
    sorted.push_back({x[i],logs[i]});
  }
  std::sort(sorted.begin(),sorted.end(),[](const Observation& a,const Observation& b){return a.x<b.x;});
  for(std::size_t i=1;i<sorted.size();++i)
    if(sorted[i].x!=sorted[i-1].x && sorted[i].y==sorted[i-1].y)return true;
  return false;
}

// e^z - 1 - z without cancellation near zero.
inline double expm1_minus(double z) {
  if (std::abs(z) < 0.1) {
    double p = 1.0 / 479001600.0;  // 1/12!
    double factorial = 479001600.0;
    for (int k = 12; k > 2; --k) {
      factorial /= k;
      p = p * z + 1.0 / factorial;
    }
    return p * z * z;
  }
  return std::expm1(z) - z;
}

// log(a) - digamma(a) > 0, with the asymptotic series where the direct
// difference cancels.
inline double log_minus_digamma(double a) {
  if (a >= 100.0) {
    const double r = 1.0 / a, r2 = r * r;
    return 0.5 * r +
           r2 * (1.0 / 12.0 +
                 r2 * (-1.0 / 120.0 +
                       r2 * (1.0 / 252.0 + r2 * (-1.0 / 240.0 + r2 / 132.0))));
  }
  return std::log(a) - special::digamma(a);
}

// d/da (log(a) - digamma(a)) = 1/a - trigamma(a) < 0.
inline double dlog_minus_digamma(double a) {
  if (a >= 100.0) {
    const double r = 1.0 / a, r2 = r * r;
    return -r2 * (0.5 + r * (1.0 / 6.0 +
                             r2 * (-1.0 / 30.0 + r2 * (1.0 / 42.0 - r2 / 30.0))));
  }
  return 1.0 / a - special::trigamma(a);
}

// lgamma(a) - (a - 1/2) log(a) + a - log(2 pi)/2.
inline double stirling_remainder(double a) {
  if (a >= 10.0) {
    const double r = 1.0 / a, r2 = r * r;
    return r * (1.0 / 12.0 +
                r2 * (-1.0 / 360.0 +
                      r2 * (1.0 / 1260.0 +
                            r2 * (-1.0 / 1680.0 +
                                  r2 * (1.0 / 1188.0 + r2 * (-691.0 / 360360.0))))));
  }
  return std::lgamma(a) - (a - 0.5) * std::log(a) + a - kHalfLog2Pi;
}

// Gamma shape a > 0 solving log(a) - digamma(a) = s > 0: Newton in log(a)
// (the equation is decreasing and convex there).
inline double gamma_shape(double s, int &iterations,int maxit=200) {
  maxit = static_cast<int>(solver_budget(maxit));
  if (!(s > 0.0) || !std::isfinite(s)) return std::numeric_limits<double>::quiet_NaN();
  double a = s < 3.0
                 ? (3.0 - s + std::sqrt((s - 3.0) * (s - 3.0) + 24.0 * s)) / (12.0 * s)
                 : 1.0 / (s + std::log(s) - kEulerGamma);
  double b = std::log(a);
  int it = 0;
  for (; it < solver_budget(maxit); ++it) {
    a = std::exp(b);
    const double f = log_minus_digamma(a) - s;
    const double df = a * dlog_minus_digamma(a);
    if (f == 0.0 || !(df < 0.0)) break;
    double step = f / df;
    step = std::max(-5.0, std::min(5.0, step));
    b -= step;
    if (std::abs(step) <= 4e-16 * (1.0 + std::abs(b))) break;
  }
  iterations += it + 1;
  a=std::exp(b);
  if(!(a>0 && std::isfinite(a)) || std::abs(log_minus_digamma(a)-s)>1e-12*s)
    return std::numeric_limits<double>::quiet_NaN();
  return a;
}

// Standardized log data u = (y - mean) / sd, mean(u) = 0, mean(u^2) = 1.
struct LogData {
  std::vector<double> u;
  double mean = 0.0, sd = 0.0, sum = 0.0;
  double umax = 0.0, umin = 0.0, kappa3 = 0.0;
  std::size_t n = 0;
};

inline bool standardize(span y, LogData &d) {
  d.n = y.size();
  if (d.n < 2) return false;
  CPPUNIV_EXTENDED_REAL total = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (double v : y) {
    if (!std::isfinite(v)) return false;
    total += v;
  }
  const double dn = static_cast<double>(d.n);
  d.sum = static_cast<double>(total);
  d.mean = static_cast<double>(total / static_cast<CPPUNIV_EXTENDED_REAL>(d.n));
  d.u.resize(d.n);
  CPPUNIV_EXTENDED_REAL shift = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (std::size_t i = 0; i < d.n; ++i) {
    d.u[i] = y[i] - d.mean;
    shift += d.u[i];
  }
  const double correction = static_cast<double>(shift / static_cast<CPPUNIV_EXTENDED_REAL>(d.n));
  CPPUNIV_EXTENDED_REAL squares = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (double &v : d.u) {
    v -= correction;
    squares += static_cast<CPPUNIV_EXTENDED_REAL>(v) * v;
  }
  d.mean += correction;
  d.sd = std::sqrt(static_cast<double>(squares / static_cast<CPPUNIV_EXTENDED_REAL>(d.n)));
  if (!(d.sd > 0.0) || !std::isfinite(d.sd)) return false;
  CPPUNIV_EXTENDED_REAL cubes = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  d.umax = -std::numeric_limits<double>::infinity();
  d.umin = std::numeric_limits<double>::infinity();
  for (double &v : d.u) {
    v /= d.sd;
    cubes += static_cast<CPPUNIV_EXTENDED_REAL>(v) * v * v;
    d.umax = std::max(d.umax, v);
    d.umin = std::min(d.umin, v);
  }
  d.kappa3 = static_cast<double>(cubes) / dn;
  // A sorted scan can stop once all remaining tilted weights underflow;
  // this matters in the power-face tail without an n-dependent truncation error.
  std::sort(d.u.begin(), d.u.end(), [](double a, double b) { return a > b; });
  return d.umax > 0.0;
}

// Maximised log likelihoods of the two faces (suprema of the family).
inline double lognormal_loglik(const LogData &d) {
  const double dn = static_cast<double>(d.n);
  return -dn * (kHalfLog2Pi + std::log(d.sd) + 0.5) - d.sum;
}
inline double power_delta(const LogData &d) {
  return kHalfLog2Pi - 0.5 - std::log(d.umax);
}

// K(t) = log mean exp(t u) and its first two derivatives.
struct Cgf {
  double K, K1, K2;
};

inline Cgf cgf(const LogData &d, double t) {
  const double dn = static_cast<double>(d.n);
  Cgf c{0.0, 0.0, 0.0};
  if (t * d.umax <= 30.0) {
    // mean(u) = 0 exactly after centring: mean(exp(t u)) = 1 + mean(e2(t u)).
    double m2 = 0.0, s1 = 0.0, s2 = 0.0;
    for (double v : d.u) {
      const double z = t * v;
      const double e2 = z < -50.0 ? -1.0 - z : expm1_minus(z);  // one transcendental call per term
      const double e1 = e2 + z;          // expm1(z)
      m2 += e2;
      s1 += v * e1;
      s2 += v * v * (1.0 + e1);
    }
    m2 /= dn;
    s1 /= dn;
    s2 /= dn;
    c.K = std::log1p(m2);
    c.K1 = s1 / (1.0 + m2);
    c.K2 = s2 / (1.0 + m2) - c.K1 * c.K1;
  } else {
    double w0 = 0.0, w1 = 0.0, w2 = 0.0;
    for (double v : d.u) {
      const double dv = v - d.umax;
      const double z = t * dv;
      if (z < -745.0) break;
      const double w = std::exp(z);
      w0 += w;
      w1 += dv * w;
      w2 += dv * dv * w;
    }
    const double a1 = w1 / w0;
    c.K = t * d.umax + std::log(w0 / dn);
    c.K1 = d.umax + a1;
    c.K2 = w2 / w0 - a1 * a1;
  }
  c.K2 = std::max(c.K2, 0.0);
  return c;
}

inline double delta_at(double t, double a, double K) {
  return std::log(t) + 0.5 * std::log(a) - stirling_remainder(a) - a * K + 0.5;
}

struct ProfilePoint {
  double t, delta, slope, shape, K, K1;
};

// z*expm1(z)-2*(expm1(z)-z), with its quadratic term cancelled
// algebraically. This is used only for |z|<=0.1.
inline double cubic_exponential_remainder(double z) {
  return z*z*z*(1.0/6+z*(2.0/24+z*(3.0/120+z*(4.0/720+z*(5.0/5040+z*(6.0/40320+z*(7.0/362880+z*(8.0/3628800+z*(9.0/39916800+z*(10.0/479001600))))))))));
}
// log(1+m)-m/(1+m), m<=0.006, starting with m^2/2.
inline double log_fraction_remainder(double m) {
  return m*m*(1.0/2+m*(-2.0/3+m*(3.0/4+m*(-4.0/5+m*(5.0/6+m*(-6.0/7+m*(7.0/8+m*(-8.0/9+m*(9.0/10+m*(-10.0/11+m*(11.0/12)))))))))));
}

inline double normal_profile_slope(const LogData& d,double t,double a) {
  double m=0;
  CPPUNIV_EXTENDED_REAL third=0;
  for(double u:d.u) {
    const double z=t*u;
    m+=expm1_minus(z);
    third+=cubic_exponential_remainder(z);
  }
  m/=static_cast<double>(d.n);
  const double D=static_cast<double>(third)/d.n/(1+m)-2*log_fraction_remainder(m);
  const double r=1/a,r2=r*r;
  const double correction=r*(1.0/6+r2*(-1.0/60+r2*(1.0/126+r2*(-1.0/120+r2/66))));
  // 1-a*t*K' = -(2*a*(log(a)-psi(a))-1)-a*(t*K'-2*K).
  // Evaluating both remainders avoids subtracting two terms of size 1/t.
  return (-correction-a*D)/t;
}

inline ProfilePoint profile(const LogData &d, double t, int &evaluations,int maxit=200) {
  maxit = static_cast<int>(solver_budget(maxit));
  const Cgf c = cgf(d, t);
  int inner_iterations = 0;
  const double a = gamma_shape(c.K, inner_iterations,maxit);
  ++evaluations;
  const double slope=a>=100 && t*std::max(d.umax,-d.umin)<=0.1 ?
    normal_profile_slope(d,t,a) : 1/t-a*c.K1;
  return {t,delta_at(t,a,c.K),slope,a,c.K,c.K1};
}

// Brent's zero finder on [lo, hi], f(lo) and f(hi) of opposite signs.
template <typename F>
inline double brent_zero(F f, double lo, double hi, double flo, double fhi,
                         double tol, int maxit, bool &converged) {
  maxit = static_cast<int>(solver_budget(maxit));
  double a = lo, b = hi, fa = flo, fb = fhi, c = a, fc = fa, d = b - a, e = d;
  converged = false;
  for (int it = 0; it < maxit; ++it) {
    if ((fb > 0.0) == (fc > 0.0)) {
      c = a;
      fc = fa;
      d = e = b - a;
    }
    if (std::abs(fc) < std::abs(fb)) {
      a = b; b = c; c = a;
      fa = fb; fb = fc; fc = fa;
    }
    const double tol1 = 2.0 * 2.2e-16 * std::abs(b) + 0.5 * tol;
    const double m = 0.5 * (c - b);
    if (std::abs(m) <= tol1 || fb == 0.0) {
      converged = true;
      return b;
    }
    if (std::abs(e) >= tol1 && std::abs(fa) > std::abs(fb)) {
      double p, q, r;
      const double s = fb / fa;
      if (a == c) {
        p = 2.0 * m * s;
        q = 1.0 - s;
      } else {
        q = fa / fc;
        r = fb / fc;
        p = s * (2.0 * m * q * (q - r) - (b - a) * (r - 1.0));
        q = (q - 1.0) * (r - 1.0) * (s - 1.0);
      }
      if (p > 0.0) q = -q; else p = -p;
      if (2.0 * p < std::min(3.0 * m * q - std::abs(tol1 * q), std::abs(e * q))) {
        e = d;
        d = p / q;
      } else {
        d = m;
        e = m;
      }
    } else {
      d = m;
      e = m;
    }
    a = b;
    fa = fb;
    b += std::abs(d) > tol1 ? d : (m > 0.0 ? tol1 : -tol1);
    fb = f(b);
  }
  return b;
}

inline TrGammaResult make_result(const LogData &d, double shape1, double t, double K,
                                 double delta, int iterations, TrGammaStatus status) {
  const double log_scale = d.mean + d.sd * (K - std::log(shape1)) / t;
  const double logLik = static_cast<double>(d.n) * delta + lognormal_loglik(d);
  return {shape1, t / d.sd, std::exp(log_scale), log_scale, logLik, iterations, status};
}

// Fixed shape1 = a: Delta_a(t) = log(t) + log(a)/2 - stirling(a) - a K(t) + 1/2
// is strictly concave in b = log(t): its derivative 1 - a t K'(t) decreases
// from 1 to -inf. Safeguarded Newton on that derivative inside a bracket.
inline TrGammaResult fit_fixed_shape1(const LogData &d, double a, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  int evaluations = 0;
  auto score = [&](double b, double &derivative) {
    const double t = std::exp(b);
    const Cgf c = cgf(d, t);
    ++evaluations;
    derivative = -a * t * (c.K1 + t * c.K2);
    return 1.0 - a * t * c.K1;
  };
  const double bmin = std::log(kMinT), bmax = 700.0;
  double b = std::max(bmin, std::min(bmax, -0.5 * std::log(a)));
  double lo = bmin, hi = bmax;
  double unused = 0.0;
  const bool rising = score(b, unused) > 0.0;
  if (rising) lo = b; else hi = b;
  for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && lo < bmax && hi > bmin && (rising ? hi == bmax : lo == bmin); ++bracket_pass) {
    const double next = rising ? std::min(bmax, lo + 2.0) : std::max(bmin, hi - 2.0);
    if (score(next, unused) > 0.0) lo = next; else hi = next;
    if ((rising && next >= bmax) || (!rising && next <= bmin)) break;
  }
  if(!(score(lo,unused)>0 && score(hi,unused)<0))return make_result(d,a,std::exp(b),cgf(d,std::exp(b)).K,0,evaluations,TrGammaStatus::IterationFailure);
  b = 0.5 * (lo + hi);
  bool converged = false;
  for (int it = 0; it < maxit; ++it) {
    double df = 0.0;
    const double f = score(b, df);
    if (f == 0.0) { converged = true; break; }
    if (f > 0.0) lo = b; else hi = b;
    double next = df < 0.0 ? b - f / df : 0.5 * (lo + hi);
    if (!(next > lo && next < hi)) next = 0.5 * (lo + hi);
    const double step = next - b;
    b = next;
    if (std::abs(step) <= tol || f == 0.0 || hi - lo <= tol) {
      converged = true;
      break;
    }
  }
  const double t = std::exp(b);
  const Cgf c = cgf(d, t);
  return make_result(d, a, t, c.K, delta_at(t, a, c.K), evaluations,
                     converged ? TrGammaStatus::Ok : TrGammaStatus::IterationFailure);
}

inline TrGammaResult face_lognormal(const LogData &d, int iterations) {
  const double inf = std::numeric_limits<double>::infinity();
  TrGammaResult r={inf,0.0,0.0,-inf,lognormal_loglik(d),iterations,TrGammaStatus::LognormalBoundary};
  r.stop_detail=limit_detail({d.mean,d.sd},r.logLik,StopModel::Lnorm);return r;
}

inline TrGammaResult face_power(const LogData &d, int iterations) {
  const double inf = std::numeric_limits<double>::infinity();
  const double log_scale = d.mean + d.sd * d.umax;
  TrGammaResult r={0.0,inf,std::exp(log_scale),log_scale,static_cast<double>(d.n)*power_delta(d)+lognormal_loglik(d),iterations,TrGammaStatus::PowerBoundary};
  r.stop_detail=limit_detail({r.scale,1/(d.sd*d.umax)},r.logLik,StopModel::Power);return r;
}

// Evaluate the represented coefficients, not merely the profiled log-scale.
// z=log(u/a) gives a cancellation-resistant density even at large shape.
struct RepresentedLogLik {double value,pad;};
inline RepresentedLogLik represented_loglik(span y,double a,double p,double logscale) {
  CPPUNIV_EXTENDED_REAL total=0,absolute=0,uncertainty=0;
  const double eps=std::numeric_limits<double>::epsilon(),loga=std::log(a);
  const double base=std::log(p)+0.5*loga-kHalfLog2Pi-stirling_remainder(a);
  for(double v:y) {
    const double z=p*(v-logscale)-loga,penalty=a*expm1_minus(z);
    const double term=base-v-penalty;
    if(!std::isfinite(term))return {NAN,INFINITY};
    total+=term;absolute+=std::abs(base)+std::abs(v)+std::abs(penalty);
    const double dz=8*eps*(1+std::abs(p)*(std::abs(v)+std::abs(logscale))+std::abs(loga));
    uncertainty+=a*std::abs(std::expm1(z))*dz+0.5*a*std::exp(z+dz)*dz*dz;
  }
  return {static_cast<double>(total),64*eps*(y.size()+static_cast<double>(absolute))+static_cast<double>(uncertainty)};
}

template <typename Result,typename Status>
inline Result check_represented(span y,Result out,Status ok,Status numerical,bool inverse=false,double jacobian=0) {
  if(out.status!=ok && out.status!=numerical)return out;
  const double sign=inverse?-1.0:1.0;
  auto at=[&](double a,double p,double scale) {
    auto point=represented_loglik(y,a,p,sign*std::log(scale));
    point.value+=jacobian;
    point.pad+=64*std::numeric_limits<double>::epsilon()*(1+std::abs(jacobian));
    return point;
  };
  std::vector<FitCandidate> candidates;
  for(auto c:out.stop_detail.candidates) {
    if(c.model==StopModel::None && c.coef.size()==3) {
      if(!valid_stop_coefficients(c.model,c.coef))continue;
      const auto point=at(c.coef[0],c.coef[1],c.coef[2]);
      if(!std::isfinite(point.value) || !std::isfinite(point.pad))continue;
      c.loglik_lower=point.value-point.pad;c.loglik_upper=point.value+point.pad;
    }
    candidates.push_back(c);
  }
  out.stop_detail.candidates=candidates;
  if(out.shape1>0 && out.shape2>0 && out.scale>0 &&
     std::isfinite(out.shape1) && std::isfinite(out.shape2) && std::isfinite(out.scale)) {
    const auto point=at(out.shape1,out.shape2,out.scale);
    if(out.status==ok && (!std::isfinite(point.value) || !std::isfinite(point.pad) ||
       std::abs(point.value-out.logLik)+point.pad>1e-10*(1+std::abs(out.logLik)))) {
      out.status=numerical;out.reconstruction_failure=true;out.stop_detail.loglik_upper=INFINITY;
    }
    if(std::isfinite(point.value)) {
      out.logLik=point.value;
      if(out.status==numerical && std::isfinite(point.pad) && out.stop_detail.candidates.empty())
        out.stop_detail.candidates.push_back({{out.shape1,out.shape2,out.scale},point.value-point.pad,point.value+point.pad,StopModel::None});
    }
  }
  if(out.status==numerical) {
    double lower=std::isfinite(out.stop_detail.limit_loglik)?out.stop_detail.limit_loglik:-INFINITY;
    for(const auto& c:out.stop_detail.candidates)lower=std::max(lower,c.loglik_lower);
    out.stop_detail.loglik_lower=std::isfinite(lower)?lower:NAN;
  }
  return out;
}

// Uniform bound after refitting both normal nuisance coordinates. Taylor's
// integral remainder bounds the fourth moment before log; the alternating
// Stirling bound R(a)>=1/(12a)-1/(360a^3) closes the zero-skew seam.
inline double normal_seam_upper(const LogData& d,double T) {
  double fourth=0;
  for(double u:d.u)fourth+=u*u*u*u/static_cast<double>(d.n);
  const double B=std::max(d.umax,-d.umin),h=T*B;
  const double c=fourth*std::exp(-h)/12-std::exp(2*h)/4;
  const double Dmin=1-std::abs(d.kappa3)*T/3-std::abs(c)*T*T;
  if(!(Dmin>0) || T*T*std::exp(h)/2>0.5)return INFINITY;
  const double C=-c/2+std::pow(std::abs(d.kappa3)/3+std::abs(c)*T,2)/(4*Dmin*Dmin)
    -Dmin/(6*(1+std::sqrt(1+2*T*T*std::exp(h)/3)))+std::pow(T,4)*std::exp(3*h)/360;
  const double A=-d.kappa3/6;
  double upper=std::max(0.0,A*T+C*T*T);
  if(C<0 && A>0 && -A/(2*C)<T)upper=std::max(upper,-A*A/(4*C));
  return upper;
}

// Free fit: one-dimensional search in t with an exact inner gamma fit.
inline TrGammaResult fit_free(const LogData &d, double tol, int maxit, std::int64_t budget=-1) {
  if(budget<0)budget=certificate_budget(maxit);
  maxit = static_cast<int>(solver_budget(maxit));
  int evaluations = 0;
  const double dn = static_cast<double>(d.n);
  // Taylor's integral remainder gives K >= log(1+t^2 exp(-tB)/2).
  // R(a)>0 and max_a(.5 log(a)-aK)=-.5 log(2K)-.5 imply
  // Delta <= tB/2 + .5 log(1+t^2/2) on the omitted seam.
  const double B = std::max(d.umax, -d.umin);
  const double t_lo = kTiePerObs / (4.0 * B);
  // Beyond t_hi, K(t) >= t*max(u) - log(n) bounds Delta below the power face.
  const double t_hi = 4.0 * (dn + std::log(dn) + 10.0) / d.umax;
  const int per_decade = 16;
  const int cells = std::max(8, static_cast<int>(std::ceil(per_decade * std::log10(t_hi / t_lo))));

  ProfilePoint best{0.0, -std::numeric_limits<double>::infinity(), 0.0, 0.0, 0.0, 0.0};
  bool failure = false, budget_exhausted=false, coefficient_failure=false;
  std::int64_t checked=0;
  auto consider = [&](const ProfilePoint &p) {
    if(!std::isfinite(p.delta) || !std::isfinite(p.slope) || !std::isfinite(p.shape))return;
    if (p.delta > best.delta) best = p;
  };
  auto refine_log = [&](const ProfilePoint &l, const ProfilePoint &r) {
    auto f = [&](double b) {
      const double t = std::exp(b);
      return profile(d, t, evaluations,maxit).slope * t;
    };
    bool ok = false;
    const double b = brent_zero(f, std::log(l.t), std::log(r.t), l.slope * l.t,
                                r.slope * r.t, std::min(tol, 1e-13), maxit, ok);
    // A proposal need not converge: acceptance is decided by the certificate.
    (void)ok;
    consider(profile(d, std::exp(b), evaluations,maxit));
  };

  // Start with a represented finite proposal so a very small certificate
  // cap can retain an interior witness rather than only the normal seam.
  if(budget>0){++checked;consider(profile(d,1.0,evaluations,maxit));}
  // Proposals are completed first so that every exclusion uses the same
  // incumbent. No grid sign change is used as an exclusion certificate.
  std::vector<ProfilePoint> grid;
  for (int j=0; j<=cells; ++j) {
    if(++checked>budget){failure=true;budget_exhausted=true;break;}
    auto p=profile(d,t_lo*std::pow(t_hi/t_lo,static_cast<double>(j)/cells),evaluations,maxit);
    if(!std::isfinite(p.delta) || !std::isfinite(p.slope) || !std::isfinite(p.shape)) {
      if(j==0 || j==cells)failure=true;
      continue;
    }
    consider(p);
    if(!grid.empty() && grid.back().slope>0 && p.slope<=0)refine_log(grid.back(),p);
    grid.push_back(p);
  }
  // A local interval is accepted only after proving strict concavity and
  // opposite endpoint scores. Tilted variances obey V(r)>=exp(-h range)V(l).
  double basin_lo=0,basin_hi=0;
  if(!failure && best.delta>std::max(0.0,power_delta(d))+kTiePerObs) {
    // Near the normal seam, a fixed relative radius costs O(t^-2)
    // curvature cells. Balance that width against the practical score noise.
    const double radius=std::min(2e-4,std::max(0.01*best.t*best.t,
      4096*std::numeric_limits<double>::epsilon()/(best.t*best.t)));
    const double bl=best.t*(1-radius),br=best.t*(1+radius);
    struct Interval {double l,r;};
    std::vector<Interval> pending{{bl,br}};
    bool concave=true;
    while(!pending.empty() && concave) {
      if(++checked>budget){failure=true;budget_exhausted=true;concave=false;break;}
      const auto cell=pending.back();pending.pop_back();
      const auto pl=profile(d,cell.l,evaluations,maxit),pr=profile(d,cell.r,evaluations,maxit);
      const double variance=cgf(d,cell.l).K2*std::exp(-(cell.r-cell.l)*(d.umax-d.umin));
      const double curvature=-1/(cell.r*cell.r)+pr.K1*pr.K1/(-dlog_minus_digamma(pl.shape))-pr.shape*variance;
      if(curvature < -1e-12/(cell.r*cell.r))continue;
      const double mid=(cell.l+cell.r)/2;
      if(!(mid>cell.l && mid<cell.r)){concave=false;break;}
      pending.push_back({cell.l,mid});pending.push_back({mid,cell.r});
    }
    const auto pl=profile(d,bl,evaluations,maxit),pr=profile(d,br,evaluations,maxit);
    if(concave && pl.slope>0 && pr.slope<0){basin_lo=bl;basin_hi=br;}
    if(!(basin_hi>basin_lo))failure=true;
    else {
      double left=basin_lo,right=basin_hi;
      for(int j=0;j<maxit && right-left>1e-14*best.t;++j) {
        const auto p=profile(d,(left+right)/2,evaluations,maxit);
        if(!std::isfinite(p.slope)){failure=true;break;}
        if(p.slope>0)left=p.t;else right=p.t;
      }
      best=profile(d,(left+right)/2,evaluations,maxit);
      const auto ql=profile(d,left,evaluations,maxit),qr=profile(d,right,evaluations,maxit);
      double shape_lo=qr.shape,shape_hi=ql.shape;
      double radius=8*std::numeric_limits<double>::epsilon()*(1+std::max(std::abs(std::log(shape_lo)),std::abs(std::log(shape_hi))));
      bool enclosed=false;
      for(int j=0;j<maxit && radius<0.5;++j) {
        shape_lo=qr.shape*(1-radius);shape_hi=ql.shape*(1+radius);
        if(log_minus_digamma(shape_lo)>qr.K && log_minus_digamma(shape_hi)<ql.K){enclosed=true;break;}
        radius*=2;
      }
      // The inverse-digamma proposal can oscillate by ulps. The certified
      // enclosure, rather than the last Newton value, controls acceptance.
      if(enclosed) {
        best.shape=std::max(shape_lo,std::min(shape_hi,best.shape));
        best.delta=delta_at(best.t,best.shape,best.K);
        best.slope=best.shape>=100 && best.t*std::max(d.umax,-d.umin)<=0.1 ?
          normal_profile_slope(d,best.t,best.shape) : 1/best.t-best.shape*best.K1;
      }
      const double nl=ql.K-std::log(shape_hi),nr=qr.K-std::log(shape_lo);
      const double low=d.mean+d.sd*std::min(nl/left,nl/right);
      const double high=d.mean+d.sd*std::max(nr/left,nr/right);
      // Root conditioning may preclude a coefficient claim despite a tiny
      // objective gap; this stops rather than accepting the objective alone.
      const double fitted_log_scale=d.mean+d.sd*(best.K-std::log(best.shape))/best.t;
      const bool represented_scale=fitted_log_scale>std::log(std::numeric_limits<double>::min()) && fitted_log_scale<std::log(std::numeric_limits<double>::max());
      if(!enclosed || std::abs(log_minus_digamma(best.shape)-best.K)>1e-12*best.K || right-left>1e-14*best.t || shape_hi/shape_lo-1>2e-11 ||
         (represented_scale && high-low>1e-10))coefficient_failure=true;
    }
  }
  double seam=0.05/B;
  for(std::int64_t j=0;j<certificate_budget(maxit);++j) {
    if(++checked>budget){failure=true;budget_exhausted=true;break;}
    const double upper=normal_seam_upper(d,seam);
    const double face=std::max(0.0,power_delta(d));
    const double face_pad=64*std::numeric_limits<double>::epsilon()*(1+std::abs(best.delta)+std::abs(face));
    const double bound=best.delta>face+kTiePerObs ? best.delta-2e-13*(1+std::abs(best.delta)) :
      face+(best.delta>face+face_pad?kTiePerObs:face_pad);
    if(upper<=bound)break;
    seam/=2;
    if(j+1==certificate_budget(maxit)){failure=true;budget_exhausted=true;}
  }
  // H(K) = sup_a [a log(a)-a-lgamma(a)-a K] is convex. Its
  // secant over [K(l), K(r)] is an upper bound. Consequently the
  // profile on [l,r] is bounded by log(t)-a_sec K(t)+constant,
  // a strictly concave function of t. The intersection of its endpoint
  // tangents bounds its maximum, including peaks missed by the grid.
  auto certify = [&](const ProfilePoint &left, const ProfilePoint &right) {
    struct Cell { ProfilePoint l, r; int depth; };
    std::vector<Cell> pending{{left, right, 0}};
    while (!pending.empty()) {
      if(failure)return;
      if(++checked>budget){failure=true;budget_exhausted=true;return;}
      const Cell cell = pending.back();
      pending.pop_back();
      const auto &l = cell.l;
      const auto &r = cell.r;
      const double variance=cgf(d,l.t).K2*std::exp(-(r.t-l.t)*(d.umax-d.umin));
      const double curvature=-1/(r.t*r.t)+r.K1*r.K1/(-dlog_minus_digamma(l.shape))-r.shape*variance;
      const double rise=std::max(0.0,curvature)*(r.t-l.t);
      const double score_pad=64*std::numeric_limits<double>::epsilon()*(1+1/l.t+l.shape*l.K1);
      // Integrating an upper curvature bound from either endpoint can
      // exclude stationary maxima without resolving tiny likelihood gaps.
      if(r.slope-rise>score_pad || l.slope+rise < -score_pad)continue;
      const double da = (l.delta - r.delta + std::log(r.t / l.t)) / (r.K - l.K);
      if(!(da>0) || !std::isfinite(da)){failure=true;return;}
      const double sl = 1.0 / l.t - da * l.K1;
      const double sr = 1.0 / r.t - da * r.K1;
      double upper = std::max(l.delta, r.delta);
      if (sl > 0.0 && sr < 0.0) {
        const double offset = (r.delta - l.delta - sr * (r.t - l.t)) / (sl - sr);
        upper = l.delta + sl * std::max(0.0, std::min(r.t - l.t, offset));
      }
      const double face=std::max(0.0,power_delta(d));
      const double pad=64*std::numeric_limits<double>::epsilon()*(1+std::abs(best.delta));
      if(best.delta>face+kTiePerObs) {
        if((l.t>=basin_lo && r.t<=basin_hi) || upper<best.delta-pad)continue;
      } else {
        const double face_pad=64*std::numeric_limits<double>::epsilon()*(1+std::abs(best.delta)+std::abs(face));
        if(upper<=face+(best.delta>face+face_pad?kTiePerObs:face_pad))continue;
      }
      const double center=std::sqrt(l.t*r.t);
      if(!(center>l.t && center<r.t)){failure=true;continue;}
      const auto mid = profile(d, std::sqrt(l.t * r.t), evaluations,maxit);
      if(!std::isfinite(mid.delta) || !std::isfinite(mid.slope)){failure=true;return;}
      if(mid.delta>best.delta+2e-13*(1+std::abs(best.delta))){failure=true;return;}
      pending.push_back({l, mid, cell.depth + 1});
      pending.push_back({mid, r, cell.depth + 1});
    }
  };
  ProfilePoint prev=profile(d,seam,evaluations,maxit);
  for(const auto& p:grid)if(p.t>seam && !failure) {certify(prev,p);prev=p;}

  if(budget_exhausted || failure || coefficient_failure) {
    auto out=make_result(d,best.shape,best.t,best.K,best.delta,evaluations,TrGammaStatus::IterationFailure);
    out.stop_detail.budget_exhausted=budget_exhausted;
    out.stop_detail.loglik_upper=INFINITY;
    const auto face=power_delta(d)>0 ? face_power(d,evaluations) : face_lognormal(d,evaluations);
    out.stop_detail.limit_model=face.stop_detail.limit_model;
    out.stop_detail.limit_coef=face.stop_detail.limit_coef;
    out.stop_detail.limit_loglik=face.logLik;
    out.stop_detail.loglik_lower=face.logLik;
    if(std::isfinite(out.logLik) && out.log_scale>std::log(std::numeric_limits<double>::min()) && out.log_scale<std::log(std::numeric_limits<double>::max())) {
      const double pad=64*std::numeric_limits<double>::epsilon()*dn*(1+std::abs(out.logLik));
      out.stop_detail.candidates.push_back({{out.shape1,out.shape2,out.scale},out.logLik-pad,out.logLik+pad,StopModel::None});
      out.stop_detail.loglik_lower=std::max(face.logLik,out.logLik-pad);
    }
    return out;
  }
  const double face=std::max(0.0,power_delta(d));
  const double face_pad=64*std::numeric_limits<double>::epsilon()*(1+std::abs(best.delta)+std::abs(face));
  if(best.delta<=face+kTiePerObs) {
    auto endpoint=power_delta(d)>0 ? face_power(d,evaluations) : face_lognormal(d,evaluations);
    if(best.delta>face+face_pad) {
      auto out=make_result(d,best.shape,best.t,best.K,best.delta,evaluations,TrGammaStatus::IterationFailure);
      out.stop_detail=endpoint.stop_detail;
      out.stop_detail.comparison_tie=true;out.stop_detail.limit_loglik=endpoint.logLik;
      out.stop_detail.loglik_lower=endpoint.logLik;
      out.stop_detail.loglik_upper=endpoint.logLik+dn*kTiePerObs+64*std::numeric_limits<double>::epsilon()*(1+std::abs(endpoint.logLik));
      if(out.log_scale>std::log(std::numeric_limits<double>::min()) && out.log_scale<std::log(std::numeric_limits<double>::max())) {
        const double pad=64*std::numeric_limits<double>::epsilon()*dn*(1+std::abs(out.logLik));
        out.stop_detail.candidates.insert(out.stop_detail.candidates.begin(),{{out.shape1,out.shape2,out.scale},out.logLik-pad,out.logLik+pad,StopModel::None});
        out.stop_detail.loglik_lower=std::max(endpoint.logLik,out.logLik-pad);
      }
      return out;
    }
    endpoint.stop_detail.loglik_lower=endpoint.logLik;
    endpoint.stop_detail.loglik_upper=endpoint.logLik+dn*face_pad+64*std::numeric_limits<double>::epsilon()*(1+std::abs(endpoint.logLik));
    return endpoint;
  }
  return make_result(d, best.shape, best.t, best.K, best.delta, evaluations,
                     failure ? TrGammaStatus::IterationFailure : TrGammaStatus::Ok);
}

}  // namespace trgamma_detail

// Public face-only helper for other positive-power families (e.g. GB2).
// The endpoint and shape describe the power face without the finite-sample
// offset used by mlpower; invalid input returns valid=false.
struct TrGammaFaces {
  bool valid;
  double lognormal_logLik, power_logLik, meanlog, sdlog, power_shape, log_endpoint;
};

inline TrGammaFaces trgamma_faces_log(span y) {
  trgamma_detail::LogData d;
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (!trgamma_detail::standardize(y, d)) return {false, nan, nan, nan, nan, nan, nan};
  const double ln = trgamma_detail::lognormal_loglik(d);
  return {true, ln, ln + y.size() * trgamma_detail::power_delta(d), d.mean, d.sd,
          1.0 / (d.sd * d.umax), d.mean + d.sd * d.umax};
}

// Fit on log data y = log(x). NaN shape1/shape2 are estimated, positive
// finite values are held fixed. logLik is for x = exp(y).
inline TrGammaResult trgamma_fit_log(span y, double tol, int maxit,
                                     double shape1 = std::numeric_limits<double>::quiet_NaN(),
                                     double shape2 = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan = std::numeric_limits<double>::quiet_NaN();
  TrGammaResult bad{shape1, shape2, nan, nan, nan, 0, TrGammaStatus::InvalidData};
  for (double s : {shape1, shape2}) {
    if (!std::isnan(s) && (!(s > 0.0) || !std::isfinite(s))) {
      bad.status = TrGammaStatus::InvalidFixed;
      return bad;
    }
  }
  if (!(tol > 0.0) || !std::isfinite(tol) || maxit < 1) return bad;
  trgamma_detail::LogData d;
  const bool standardized = trgamma_detail::standardize(y, d);
  // Both shapes held: only scale is fitted, even for a singleton or
  // constant sample. Centre before exponentiating to avoid x^shape2.
  if (!standardized && !std::isnan(shape1) && !std::isnan(shape2)) {
    if (y.size() == 0) return bad;
    double ymax = -std::numeric_limits<double>::infinity();
    CPPUNIV_EXTENDED_REAL sum_y = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
    for (double v : y) {
      if (!std::isfinite(v)) return bad;
      ymax = std::max(ymax, v);
      sum_y += v;
    }
    CPPUNIV_EXTENDED_REAL total = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
    for (double v : y) total += std::exp(shape2 * (v - ymax));
    const double log_scale = ymax +
        (std::log(static_cast<double>(total / y.size())) - std::log(shape1)) / shape2;
    const double K = shape2 * (ymax - static_cast<double>(sum_y / y.size())) +
        std::log(static_cast<double>(total / y.size()));
    const double ll = y.size() * (std::log(shape2) + 0.5 * std::log(shape1) -
        trgamma_detail::kHalfLog2Pi - trgamma_detail::stirling_remainder(shape1) - shape1 * K) -
        static_cast<double>(sum_y);
    TrGammaResult result{shape1,shape2,std::exp(log_scale),log_scale,ll,0,TrGammaStatus::Ok};
    return trgamma_detail::check_represented(y,result,TrGammaStatus::Ok,TrGammaStatus::IterationFailure);
  }
  if (!standardized) return bad;
  if (!std::isnan(shape2)) {
    const double t = shape2 * d.sd;
    const trgamma_detail::Cgf c = trgamma_detail::cgf(d, t);
    int iterations = 0;
    const double a = std::isnan(shape1) ? trgamma_detail::gamma_shape(c.K, iterations,maxit) : shape1;
    if(!std::isfinite(a)){bad.status=TrGammaStatus::IterationFailure;return bad;}
    auto result = trgamma_detail::make_result(d, a, t, c.K, trgamma_detail::delta_at(t, a, c.K),
                                       iterations, TrGammaStatus::Ok);
    result.shape2 = shape2;
    return trgamma_detail::check_represented(y,result,TrGammaStatus::Ok,TrGammaStatus::IterationFailure);
  }
  auto result=!std::isnan(shape1) ? trgamma_detail::fit_fixed_shape1(d,shape1,tol,maxit) : trgamma_detail::fit_free(d,tol,maxit);
  return trgamma_detail::check_represented(y,result,TrGammaStatus::Ok,TrGammaStatus::IterationFailure);
}

// Interior fits whose scale is not a normal positive double (close to the
// lognormal face) report ScaleOutOfRange, keeping shapes and logLik.
inline TrGammaStatus trgamma_range_status(TrGammaStatus status, double log_scale) {
  if (status != TrGammaStatus::Ok) return status;
  const double lo = std::log(std::numeric_limits<double>::min());
  const double hi = std::log(std::numeric_limits<double>::max());
  return (log_scale > lo && log_scale < hi) ? status : TrGammaStatus::ScaleOutOfRange;
}

inline TrGammaResult trgamma_mle(span x, double tol = 1e-10, int maxit = 100,
                                 double shape1 = std::numeric_limits<double>::quiet_NaN(),
                                 double shape2 = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  std::vector<double> y(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) {
    if (!(x[i] > 0.0) || !std::isfinite(x[i])) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      return {shape1, shape2, nan, nan, nan, 0, TrGammaStatus::InvalidData};
    }
    y[i] = std::log(x[i]);
  }
  if(trgamma_detail::valid_fit_request(tol,maxit,shape1,shape2) &&
     trgamma_detail::lost_log_distinctions(x,{y.data(),y.size()})) {
    const double nan=std::numeric_limits<double>::quiet_NaN();
    TrGammaResult out{shape1,shape2,nan,nan,nan,0,TrGammaStatus::IterationFailure};
    out.transformation_failure=true;out.stop_detail.loglik_upper=INFINITY;return out;
  }
  TrGammaResult r = trgamma_fit_log(span{y.data(), y.size()}, tol, maxit, shape1, shape2);
  if(!x.empty() && (r.status==TrGammaStatus::PowerBoundary ||
      (r.stop_detail.limit_model==StopModel::None && std::isfinite(r.stop_detail.limit_loglik)))) {
    const auto faces=trgamma_faces_log({y.data(),y.size()});
    const double endpoint=*std::max_element(x.begin(),x.end());
    if(faces.valid && faces.power_logLik>faces.lognormal_logLik) {
      if(r.status==TrGammaStatus::PowerBoundary) {
        const double lower=r.stop_detail.loglik_lower,upper=r.stop_detail.loglik_upper;
        r.stop_detail=limit_detail({endpoint,faces.power_shape},r.logLik,StopModel::Power);
        r.stop_detail.loglik_lower=lower;r.stop_detail.loglik_upper=upper;
      } else {
        r.stop_detail.limit_model=StopModel::Power;
        r.stop_detail.limit_coef={endpoint,faces.power_shape};r.stop_detail.limit_loglik=faces.power_logLik;
      }
    }
  }
  if(r.stop_detail.comparison_tie && r.stop_detail.limit_model==StopModel::Power &&
     std::none_of(r.stop_detail.candidates.begin(),r.stop_detail.candidates.end(),[](const FitCandidate& c){return c.model==StopModel::Power;}))
    r.stop_detail.candidates.push_back({r.stop_detail.limit_coef,r.stop_detail.limit_loglik,r.stop_detail.limit_loglik,StopModel::Power});
  if(r.stop_detail.limit_model==StopModel::Power && !x.empty()) {
    const double endpoint=*std::max_element(x.begin(),x.end());
    if(r.stop_detail.limit_coef.size()==2)r.stop_detail.limit_coef[0]=endpoint;
    for(auto& c:r.stop_detail.candidates)if(c.model==StopModel::Power && c.coef.size()==2)c.coef[0]=endpoint;
    if(r.status==TrGammaStatus::PowerBoundary){r.scale=endpoint;r.log_scale=std::log(endpoint);}
  }
  r.status = trgamma_range_status(r.status, r.log_scale);
  return r;
}

}  // namespace cppunivariate

namespace cppunivariate {

// Fixed-scale data are accepted in logarithmic coordinates so reciprocal
// fits never form 1/x or 1/scale.
inline TrGammaResult trgamma_fixed_scale_log(span y,double logscale,double a,double b,double tol,int maxit,std::int64_t budget=-1) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  TrGammaResult bad{a,b,std::exp(logscale),logscale,nan,0,TrGammaStatus::InvalidData};
  if(y.empty() || !(tol>0) || !std::isfinite(tol) || maxit<1)return bad;
  maxit=static_cast<int>(solver_budget(maxit));
  if(!std::isfinite(logscale)) {bad.status=TrGammaStatus::InvalidFixed;return bad;}
  for(double v:{a,b})if(!std::isnan(v) && (!(v>0)||!std::isfinite(v))) {bad.status=TrGammaStatus::InvalidFixed;return bad;}
  std::vector<double> w;
  double sum=0,jac=0,unit=0,mx=-INFINITY,mn=INFINITY;
  for(double v:y) {
    if(!std::isfinite(v))return bad;
    const double z=v-logscale;
    w.push_back(z);sum+=z;jac-=v;unit=std::max(unit,std::abs(z));mx=std::max(mx,z);mn=std::min(mn,z);
  }
  const double n=static_cast<double>(y.size());
  // A free power can concentrate at any common x>=scale when shape is
  // also free; for a held shape concentration requires x=scale.
  if(std::isnan(b) && mx==mn && (mx==0 || (std::isnan(a)&&mx>0))) {bad.logLik=INFINITY;return bad;}
  if(unit==0)unit=1;
  for(double& v:w)v/=unit;
  const double m=sum/(n*unit),M=mx/unit;
  const double shift=jac/n-std::log(unit);
  int evaluations=0;
  bool numerical=false;
  struct Point {double q,k,value,score,variance;};
  auto at=[&](double q) {
    ++evaluations;
    double k=a;
    if(std::isnan(k)) {
      const double target=q*m;
      double u=target>=-2 ? std::log(std::exp(target)+0.5) : -std::log(-target-trgamma_detail::kEulerGamma);
      for(int j=0;j<maxit;++j) {
        k=std::exp(u);
        const double step=(special::digamma(k)-target)/(k*special::trigamma(k));
        if(!std::isfinite(step))break;
        u-=std::max(-2.0,std::min(2.0,step));
        if(std::abs(step)<2e-15)break;
      }
      k=std::exp(u);
      if(!(k>0) || !std::isfinite(k) || std::abs(special::digamma(k)-target)>1e-12*(1+std::abs(target)))numerical=true;
    }
    double ex=0,ex1=0,ex2=0;
    for(double v:w) {const double e=std::exp(q*v);ex+=e;ex1+=v*e;ex2+=v*v*e;}
    const double value=std::log(q)-special::lgamma(k)+k*q*m-ex/n+shift;
    const double score=std::isnan(a) && k<0.1 ?
      k*special::digamma(1+k)/q-ex1/n : 1/q+k*m-ex1/n;
    const double rounding=32*std::numeric_limits<double>::epsilon()*(1+std::abs(special::lgamma(k))+std::abs(k*q*m)+ex/n);
    if(!std::isfinite(value) || !std::isfinite(score) || rounding>2e-12*(1+std::abs(value)))numerical=true;
    return Point{q,k,value,score,ex2/n};
  };
  auto result=[&](const Point& p,TrGammaStatus status) {
    return TrGammaResult{p.k,p.q/unit,std::exp(logscale),logscale,n*p.value,evaluations,status};
  };
  if(!std::isnan(b)) {
    auto p=at(b*unit);
    return result(p,numerical?TrGammaStatus::IterationFailure:TrGammaStatus::Ok);
  }
  // Fixed shape: strict concavity in q, with endpoint scores +infinity
  // and negative infinity (unless every observation is at the scale).
  if(!std::isnan(a)) {
    double lo=0,hi=1;
    auto p=at(hi);
    for(int j=0;j<maxit && p.score>0 && !numerical;++j) {lo=hi;hi*=2;p=at(hi);}
    if(numerical || !(p.score<0))return result(p,TrGammaStatus::IterationFailure);
    for(int j=0;j<maxit;++j) {
      p=at((lo+hi)/2);
      if(numerical)break;
      if(p.score>0)lo=p.q;else hi=p.q;
      if(hi-lo<=std::min(tol,1e-12)*p.q)break;
    }
    return result(p,!numerical && hi-lo<=std::min(tol,1e-12)*p.q?TrGammaStatus::Ok:TrGammaStatus::IterationFailure);
  }
  const bool has_face=M<=0 && m<0;
  const double zeros=static_cast<double>(std::count(w.begin(),w.end(),0.0))/n;
  const double face=has_face ? std::log(-1/m)-1+shift-zeros : -INFINITY;
  auto best=at(1);
  auto consider=[&](const Point& p) {if(p.value>best.value)best=p;};
  bool exhausted=false;
  if(budget<0)budget=certificate_budget(maxit);
  std::int64_t checked=0;
  auto spend=[&]() {if(++checked>budget){exhausted=true;return false;}return true;};
  const double margin=2e-12*(1+std::abs(best.value));
  double low=1e-6,high=1;
  // G(s)=sup_k(ks-lgamma(k)); G is increasing and convex.
  // log(q)+G(max(0,q*m))+shift bounds the entire small-q tail.
  for(std::int64_t j=0;j<certificate_budget(maxit);++j) {
    if(!spend())break;
    const auto p=at(low);consider(p);
    const auto endpoint=at(m>0?low:low*1e-8);
    const double s=endpoint.q*m;
    const double G=m>0 ? -special::lgamma(endpoint.k)+endpoint.k*s :
      -special::lgamma(endpoint.k)+endpoint.k*s-2*s;
    const double upper=std::log(low)+G+shift;
    if(upper<best.value-margin)break;
    low/=2;
    if(j+1==certificate_budget(maxit))exhausted=true;
  }
  // Bound the entire upper tail. For m<0 use Gamma(1+k)>=exp(-Euler*k).
  // For m>=0 use exp(s)<inverse_psi(s)<exp(s)+1 and Stirling's lower bound.
  for(std::int64_t j=0;j<certificate_budget(maxit) && !numerical && !exhausted;++j) {
    if(!spend())break;
    const auto p=at(high);consider(p);
    double upper=INFINITY;bool decreasing=false;
    if(m<0 && -high*m>trgamma_detail::kEulerGamma) {
      upper=std::log(-1/m)-1+shift-std::log1p(-trgamma_detail::kEulerGamma/(-high*m));
      if(M<=0)upper-=zeros;else upper-=std::exp(high*M)/n;
      decreasing=true;
    } else if(m>=0 && M>m && high*(M-m)>std::log(2*n)) {
      const double e=std::exp(high*M)/n;
      upper=std::log(high)+std::exp(high*m)+1+0.5*(high*m+std::log(2.0))-trgamma_detail::kHalfLog2Pi-e+shift;
      decreasing=0.5*M*e>0.5*m+1/high;
    }
    const double incumbent=std::max(best.value,face);
    if(decreasing && upper<incumbent+(best.value>face+trgamma_detail::kTiePerObs?-margin:trgamma_detail::kTiePerObs))break;
    high*=2;
    if(j+1==certificate_budget(maxit))exhausted=true;
  }
  std::vector<Point> grid;
  const int cells=std::max(8,static_cast<int>(std::ceil(16*std::log10(high/low))));
  for(int j=0;j<=cells && !numerical && !exhausted;++j) {
    if(!spend())break;
    auto p=at(low*std::pow(high/low,static_cast<double>(j)/cells));consider(p);
    if(!grid.empty() && grid.back().score>0 && p.score<0) {
      double l=grid.back().q,r=p.q;
      for(int it=0;it<maxit;++it) {
        const auto mid=at((l+r)/2);consider(mid);
        if(mid.score>0)l=mid.q;else r=mid.q;
        if(r-l<1e-13*mid.q)break;
      }
    }
    grid.push_back(p);
  }
  double basin_l=0,basin_r=0;
  if(!numerical && !exhausted && best.value>face+trgamma_detail::kTiePerObs) {
    const auto l=at(best.q*(1-1e-4)),r=at(best.q*(1+1e-4));
    const double kmax=std::max(l.k,r.k);
    double vmin=0;
    for(double v:w)vmin+=v*v*std::exp((v>0?l.q:r.q)*v)/n;
    const double curvature=-1/(r.q*r.q)+m*m/special::trigamma(kmax)-vmin;
    if(l.score>0 && r.score<0 && curvature<-1e-12/(r.q*r.q)) {
      basin_l=l.q;basin_r=r.q;
      double left=l.q,right=r.q;
      for(int j=0;j<maxit && right-left>1e-14*best.q;++j) {
        const auto p=at((left+right)/2);
        if(p.score>0)left=p.q;else right=p.q;
      }
      best=at((left+right)/2);
      const auto pl=at(left),pr=at(right);
      const double small=std::min(pl.k,pr.k),large=std::max(pl.k,pr.k);
      double radius=8*std::numeric_limits<double>::epsilon()*(1+std::max(std::abs(std::log(small)),std::abs(std::log(large))));
      double kl=small,kr=large;bool enclosed=false;
      for(int j=0;j<maxit && radius<0.5;++j) {
        kl=small*(1-radius);kr=large*(1+radius);
        if(special::digamma(kl)<std::min(left*m,right*m) && special::digamma(kr)>std::max(left*m,right*m)){enclosed=true;break;}
        radius*=2;
      }
      if(enclosed && (best.k<kl || best.k>kr)) {
        best.k=std::max(kl,std::min(kr,best.k));
        double ex=0;for(double v:w)ex+=std::exp(best.q*v);
        best.value=std::log(best.q)-special::lgamma(best.k)+best.k*best.q*m-ex/n+shift;
      }
      if(!enclosed || right-left>1e-14*best.q || std::log(kr/kl)>2e-11 ||
         std::abs(special::digamma(best.k)-best.q*m)>1e-12*(1+std::abs(best.q*m)))numerical=true;
    }
    else numerical=true;
  }
  struct Cell {Point l,r;};
  std::vector<Cell> pending;
  for(std::size_t j=1;j<grid.size();++j)pending.push_back({grid[j-1],grid[j]});
  while(!pending.empty() && !numerical && !exhausted) {
    if(!spend())break;
    const auto cell=pending.back();pending.pop_back();
    const auto l=cell.l,r=cell.r;
    // Secant of convex G(q*m), plus the concave log(q)-mean(exp(q*w)).
    const double gl=-special::lgamma(l.k)+l.k*l.q*m;
    const double gr=-special::lgamma(r.k)+r.k*r.q*m;
    const double sec=(gr-gl)/(r.q-l.q);
    if(!std::isfinite(sec)){numerical=true;break;}
    const double sl=l.score-l.k*m+sec,sr=r.score-r.k*m+sec;
    double upper=std::max(l.value,r.value);
    if(sl>0 && sr<0) {
      const double offset=(r.value-l.value-sr*(r.q-l.q))/(sl-sr);
      upper=l.value+sl*std::max(0.0,std::min(r.q-l.q,offset));
    }
    if(best.value>face+trgamma_detail::kTiePerObs) {
      if((l.q>=basin_l && r.q<=basin_r)||upper<best.value-margin)continue;
    } else if(upper<=face+trgamma_detail::kTiePerObs)continue;
    const double q=std::sqrt(l.q*r.q);
    if(!(q>l.q && q<r.q)){numerical=true;break;}
    const auto mid=at(q);
    if(mid.value>best.value+margin){numerical=true;break;}
    pending.push_back({l,mid});pending.push_back({mid,r});
  }
  auto out=result(best,TrGammaStatus::IterationFailure);
  if(exhausted || numerical) {
    out.stop_detail.budget_exhausted=exhausted;
    out.stop_detail.loglik_upper=INFINITY;
    if(std::isfinite(out.logLik) && out.shape1>0 && out.shape2>0 && out.scale>0) {
      out.stop_detail.candidates.push_back({{out.shape1,out.shape2,out.scale},out.logLik-n*margin,out.logLik+n*margin,StopModel::None});
      out.stop_detail.loglik_lower=out.logLik-n*margin;
    }
    if(has_face) {
      out.stop_detail.limit_model=StopModel::Power;
      out.stop_detail.limit_coef={std::exp(logscale),-1/(m*unit)};
      out.stop_detail.limit_loglik=n*face;
      out.stop_detail.loglik_lower=std::fmax(out.stop_detail.loglik_lower,n*face);
    }
    return out;
  }
  if(best.value<=face+trgamma_detail::kTiePerObs) {
    // At a held endpoint the finite profile approaches its face from above:
    // F(q)=face+Euler/(-q*m)+O(q^-2)-mean_{w<0} exp(q*w).
    // Thus a tolerance-level face comparison cannot prove nonattainment.
    const FitCandidate finite{{out.shape1,out.shape2,out.scale},out.logLik-n*margin,out.logLik+n*margin,StopModel::None};
    const FitCandidate endpoint{{std::exp(logscale),-1/(m*unit)},n*face,n*face,StopModel::Power};
    out.stop_detail.comparison_tie=true;
    out.stop_detail.candidates={finite,endpoint};
    out.stop_detail.loglik_lower=std::max(finite.loglik_lower,n*face);
    out.stop_detail.loglik_upper=n*(face+trgamma_detail::kTiePerObs+margin);
    return out;
  }
  out.status=TrGammaStatus::Ok;
  return out;
}

inline TrGammaResult trgamma_mle_fixed_scale(span x,double scale,double a=std::numeric_limits<double>::quiet_NaN(),double b=std::numeric_limits<double>::quiet_NaN(),double tol=1e-10,int maxit=100) {
  std::vector<double> y;
  for(double v:x)y.push_back(v>0 && std::isfinite(v)?std::log(v):NAN);
  if(!(scale>0) || !std::isfinite(scale)) {
    const double nan=std::numeric_limits<double>::quiet_NaN();
    return {a,b,scale,nan,nan,0,TrGammaStatus::InvalidFixed};
  }
  if(trgamma_detail::valid_fit_request(tol,maxit,a,b) &&
     trgamma_detail::lost_log_distinctions(x,{y.data(),y.size()},scale,std::log(scale))) {
    TrGammaResult out{a,b,scale,std::log(scale),NAN,0,TrGammaStatus::IterationFailure};
    out.transformation_failure=true;out.stop_detail.loglik_upper=INFINITY;return out;
  }
  auto out=trgamma_fixed_scale_log({y.data(),y.size()},std::log(scale),a,b,tol,maxit);
  out.scale=scale;
  if(!std::isnan(b))out.shape2=b;
  if(out.stop_detail.limit_model==StopModel::Power && out.stop_detail.limit_coef.size()==2)out.stop_detail.limit_coef[0]=scale;
  for(auto& c:out.stop_detail.candidates) {
    if(c.model==StopModel::None && c.coef.size()==3) {
      c.coef[2]=scale;if(!std::isnan(b))c.coef[1]=b;
    }
    if(c.model==StopModel::Power && c.coef.size()==2)c.coef[0]=scale;
  }
  return trgamma_detail::check_represented({y.data(),y.size()},out,TrGammaStatus::Ok,TrGammaStatus::IterationFailure);
}

}
