// Contract, retained choices, evidence and known numerical limitations:
// dev/research/evaluations/2026-10-31-step7-routines.md (repository root).
// Centralized special functions and dpqr primitives used by cppunivariate.
//
// Single seam between cppunivariate and the math backend: today everything
// routes through Boost.Math (shipped via the BH package on CRAN).
// Swap this file to retarget another backend.

#pragma once
#include "cppunivariate/core/constants.hpp"
#include "../core/precision.hpp"

#include "constants.hpp"
#include "tails.hpp"
#include <boost/math/distributions/gamma.hpp>
#include <boost/math/distributions/non_central_chi_squared.hpp>
#include <boost/math/distributions/logistic.hpp>
#include <boost/math/distributions/normal.hpp>
#include <boost/math/distributions/students_t.hpp>
#include <boost/math/special_functions/owens_t.hpp>
#include <boost/math/special_functions/digamma.hpp>
#include <boost/math/special_functions/polygamma.hpp>
#include <boost/math/special_functions/trigamma.hpp>
#include <boost/math/distributions/beta.hpp>
#include <boost/math/distributions/binomial.hpp>
#include <boost/math/distributions/inverse_gaussian.hpp>
#include <boost/math/distributions/negative_binomial.hpp>
#include <boost/math/distributions/poisson.hpp>
#include <boost/math/policies/policy.hpp>
#include <boost/math/special_functions/zeta.hpp>
#include <cmath>
#include <limits>
#include <boost/math/special_functions/gamma.hpp>

namespace cppunivariate {
namespace special {

// Ignore numeric errors: return NaN/infinity without exceptions or errno side
// effects. Keep Boost's precision defaults; round discrete quantiles like R.
typedef boost::math::policies::policy<
    boost::math::policies::domain_error<boost::math::policies::ignore_error>,
    boost::math::policies::pole_error<boost::math::policies::ignore_error>,
    boost::math::policies::overflow_error<boost::math::policies::ignore_error>,
    boost::math::policies::underflow_error<boost::math::policies::ignore_error>,
    boost::math::policies::denorm_error<boost::math::policies::ignore_error>,
    boost::math::policies::evaluation_error<boost::math::policies::ignore_error>,
    boost::math::policies::rounding_error<boost::math::policies::ignore_error>,
    boost::math::policies::indeterminate_result_error<boost::math::policies::ignore_error>,
    boost::math::policies::discrete_quantile<
        boost::math::policies::integer_round_up>> NoThrowPolicy;

inline double ibeta(double a,double b,double x) { return boost::math::ibeta(a,b,x,NoThrowPolicy{}); }
inline double ibetac(double a,double b,double x) { return boost::math::ibetac(a,b,x,NoThrowPolicy{}); }
inline double ibeta_inv(double a,double b,double x) { return boost::math::ibeta_inv(a,b,x,NoThrowPolicy{}); }
inline double ibetac_inv(double a,double b,double x) { return boost::math::ibetac_inv(a,b,x,NoThrowPolicy{}); }

inline double gamma_p(double a, double x) { return boost::math::gamma_p(a,x,NoThrowPolicy{}); }
inline double gamma_q(double a, double x) { return boost::math::gamma_q(a,x,NoThrowPolicy{}); }

inline double zeta(double s) { return boost::math::zeta(s, NoThrowPolicy{}); }

inline double pnchisq(double x, double df, double ncp, bool lower) {
  const boost::math::non_central_chi_squared_distribution<double, NoThrowPolicy> dist(df,ncp);
  return lower ? boost::math::cdf(dist,x) : boost::math::cdf(boost::math::complement(dist,x));
}

// --- log-gamma family ------------------------------------------------------

inline double digamma(double x) { return boost::math::digamma(x, NoThrowPolicy{}); }

inline double trigamma(double x) { return boost::math::trigamma(x, NoThrowPolicy{}); }

inline double tetragamma(double x) { return boost::math::polygamma(2, x, NoThrowPolicy{}); }

inline double psigamma(double x, int deriv) {
  if (deriv == 0) return boost::math::digamma(x, NoThrowPolicy{});
  if (deriv == 1) return boost::math::trigamma(x, NoThrowPolicy{});
  return boost::math::polygamma(deriv, x, NoThrowPolicy{});
}

inline double lgamma(double x) { return std::lgamma(x); }

inline double lbeta(double a, double b) {
  if (!std::isfinite(a) || !std::isfinite(b) || a <= 0.0 || b <= 0.0) {
    return kNaN;
  }
  return lgamma(a) + lgamma(b) - lgamma(a + b);
}

inline double owens_t(double h, double a) { return boost::math::owens_t(h, a, NoThrowPolicy{}); }

// --- normal density (kept inline; closed-form is fine) --------------------

inline double dnorm(double x, double mean, double sd, bool log) {
  if (!std::isfinite(sd) || sd <= 0.0) {
    return kNaN;
  }
  const double z = (x - mean) / sd;
  const double log_pref = -std::log(sd) - 0.5 * std::log(2.0 * cppunivariate::pi);
  const double log_density = log_pref - 0.5 * z * z;
  return log ? log_density : std::exp(log_density);
}

inline double dpois(double x, double lambda, bool log) {
  if (!std::isfinite(lambda) || lambda < 0.0) {
    return kNaN;
  }
  if (lambda == 0.0) {
    if (x == 0.0) {
      return log ? 0.0 : 1.0;
    }
    return log ? kNegInf : 0.0;
  }
  if (!std::isfinite(x) || x < 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double k = std::floor(x + 0.5);
  // Treat non-integers as zero probability.
  if (std::fabs(k - x) > 1e-7) {
    return log ? kNegInf : 0.0;
  }
  const double log_density =
      k * std::log(lambda) - lambda - std::lgamma(k + 1.0);
  return log ? log_density : std::exp(log_density);
}

inline double dgeom(double x, double prob, bool log) {
  if (prob <= 0.0 || prob > 1.0 || x < 0.0 || !std::isfinite(x)) {
    return kNaN;
  }
  const double k = std::floor(x + 0.5); // R's geometric is on 0,1,2,...
  if (prob == 1.0) return k == 0.0 ? (log ? 0.0 : 1.0) : (log ? kNegInf : 0.0);
  const double log_density = k * std::log1p(-prob) + std::log(prob);
  return log ? log_density : std::exp(log_density);
}

inline double dnbinom(double x, double size, double prob, bool log) {
  if (!(size >= 0.0) || !std::isfinite(size) || !(prob > 0.0) || prob > 1.0) {
    return kNaN;
  }
  if (!std::isfinite(x) || x < 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double k = std::floor(x + 0.5);
  if (std::fabs(k - x) > 1e-7) {
    return log ? kNegInf : 0.0;
  }
  if (size == 0.0 || prob == 1.0) return k == 0.0 ? (log ? 0.0 : 1.0) : (log ? kNegInf : 0.0);
  const double log_density = std::lgamma(k + size) - std::lgamma(size) -
                             std::lgamma(k + 1.0) + size * std::log(prob) +
                             k * std::log1p(-prob);
  return log ? log_density : std::exp(log_density);
}

// --- normal cdf / quantile -------------------------------------------------

inline double pnorm(double x, double mean, double sd, bool lower_tail,
                    bool log_p) {
  const boost::math::normal_distribution<double, NoThrowPolicy> d(mean, sd);
  const double p = lower_tail ? boost::math::cdf(d, x) : boost::math::cdf(boost::math::complement(d, x));
  return log_p ? std::log(p) : p;
}

inline double qnorm(double p, double mean, double sd, bool lower_tail,
                    bool log_p) {
  const boost::math::normal_distribution<double, NoThrowPolicy> d(mean, sd);
  if (log_p) p = std::exp(p);
  return lower_tail ? boost::math::quantile(d, p) : boost::math::quantile(boost::math::complement(d, p));
}

// --- logistic cdf / quantile -----------------------------------------------

inline double plogis(double x, double location, double scale, bool lower_tail,
                     bool log_p) {
  const boost::math::logistic_distribution<double, NoThrowPolicy> d(location, scale);
  const double p = lower_tail ? boost::math::cdf(d, x) : boost::math::cdf(boost::math::complement(d, x));
  return log_p ? std::log(p) : p;
}

inline double qlogis(double p, double location, double scale, bool lower_tail,
                     bool log_p) {
  const boost::math::logistic_distribution<double, NoThrowPolicy> d(location, scale);
  if (log_p) p = std::exp(p);
  return lower_tail ? boost::math::quantile(d, p) : boost::math::quantile(boost::math::complement(d, p));
}

// --- gamma pdf / cdf / quantile -------------------------------------------

inline double dgamma(double x, double shape, double scale, bool log) {
  if (!(shape > 0.0) || !(scale > 0.0) || x < 0.0) {
    return kNaN;
  }
  if (log) {
    // Closed-form log-density: tail-stable.
    if (x == 0.0) return shape == 1.0 ? -std::log(scale) : kNegInf;
    return -std::lgamma(shape) - shape * std::log(scale) +
           (shape - 1.0) * std::log(x) - x / scale;
  }
  return boost::math::pdf(boost::math::gamma_distribution<double, NoThrowPolicy>(shape, scale), x);
}

inline double pgamma(double x, double shape, double scale, bool lower_tail,
                     bool log_p) {
  const boost::math::gamma_distribution<double, NoThrowPolicy> d(shape, scale);
  const double p = lower_tail ? boost::math::cdf(d, x) : boost::math::cdf(boost::math::complement(d, x));
  return log_p ? std::log(p) : p;
}

inline double qgamma(double p, double shape, double scale, bool lower_tail,
                     bool log_p) {
  const boost::math::gamma_distribution<double, NoThrowPolicy> d(shape, scale);
  if (log_p) p = std::exp(p);
  return lower_tail ? boost::math::quantile(d, p) : boost::math::quantile(boost::math::complement(d, p));
}

// --- student-t pdf / cdf / quantile ---------------------------------------

inline double dt(double x, double df, bool log) {
  if (!(df > 0.0)) {
    return kNaN;
  }
  if (log) {
    // Closed-form log-density: tail-stable.
    return std::lgamma((df + 1.0) / 2.0) - std::lgamma(df / 2.0) -
           0.5 * std::log(df * cppunivariate::pi) -
           ((df + 1.0) / 2.0) * std::log1p(x * x / df);
  }
  return boost::math::pdf(boost::math::students_t_distribution<double, NoThrowPolicy>(df), x);
}

inline double pt(double x, double df, bool lower_tail, bool log_p) {
  const boost::math::students_t_distribution<double, NoThrowPolicy> d(df);
  const double p = lower_tail ? boost::math::cdf(d, x) : boost::math::cdf(boost::math::complement(d, x));
  return log_p ? std::log(p) : p;
}

inline double qt(double p, double df, bool lower_tail, bool log_p) {
  const boost::math::students_t_distribution<double, NoThrowPolicy> d(df);
  if (log_p) p = std::exp(p);
  return lower_tail ? boost::math::quantile(d, p) : boost::math::quantile(boost::math::complement(d, p));
}

inline double beta_pdf(double x, double a, double b) {
  const boost::math::beta_distribution<double, NoThrowPolicy> d(a, b);
  return boost::math::pdf(d, x);
}

inline double beta_cdf(double x, double a, double b, bool lower_tail) {
  const boost::math::beta_distribution<double, NoThrowPolicy> d(a, b);
  return lower_tail ? boost::math::cdf(d, x)
                    : boost::math::cdf(boost::math::complement(d, x));
}

inline double beta_quantile(double x, double a, double b, bool lower_tail) {
  const boost::math::beta_distribution<double, NoThrowPolicy> d(a, b);
  return lower_tail ? boost::math::quantile(d, x)
                    : boost::math::quantile(boost::math::complement(d, x));
}

inline double binomial_pdf(double x, double a, double b) {
  const boost::math::binomial_distribution<double, NoThrowPolicy> d(a, b);
  return boost::math::pdf(d, x);
}

inline double binomial_cdf(double x, double a, double b, bool lower_tail) {
  const boost::math::binomial_distribution<double, NoThrowPolicy> d(a, b);
  return lower_tail ? boost::math::cdf(d, x)
                    : boost::math::cdf(boost::math::complement(d, x));
}

// Boost's discrete quantile rounding can keep advancing after a failed root
// or NaN CDF. Keep its no-throw CDF seam, but own bounded integer inversion.
template <typename Cdf>
inline double discrete_cdf_quantile(double p, double maximum, bool lower, Cdf cdf) {
  return discrete_quantile(discrete_log_tails(p,lower,false),0,maximum,[&](double x) {
    const double left=cdf(x,true);
    if (!(left>=0 && left<=1)) return LogTails{kNaN,kNaN,false};
    const bool small_left=left<=.5;
    const double small=small_left ? left : cdf(x,false);
    if (!(small>=0 && small<=1)) return LogTails{kNaN,kNaN,false};
    return tails_from_log(std::log(small),small_left);
  },p,lower,false);
}

inline double binomial_quantile(double x, double a, double b, bool lower_tail) {
  if (!std::isfinite(a) || a<0 || a!=std::floor(a) || !(b>=0 && b<=1)) return kNaN;
  return discrete_cdf_quantile(x,a,lower_tail,[&](double q,bool lower) {
    return binomial_cdf(q,a,b,lower);
  });
}

inline double inverse_gaussian_pdf(double x, double a, double b) {
  const boost::math::inverse_gaussian_distribution<double, NoThrowPolicy> d(a, b);
  return boost::math::pdf(d, x);
}

inline double inverse_gaussian_cdf(double x, double a, double b, bool lower_tail) {
  const boost::math::inverse_gaussian_distribution<double, NoThrowPolicy> d(a, b);
  return lower_tail ? boost::math::cdf(d, x)
                    : boost::math::cdf(boost::math::complement(d, x));
}

inline double inverse_gaussian_quantile(double x, double a, double b, bool lower_tail) {
  const boost::math::inverse_gaussian_distribution<double, NoThrowPolicy> d(a, b);
  return lower_tail ? boost::math::quantile(d, x)
                    : boost::math::quantile(boost::math::complement(d, x));
}

inline double negative_binomial_pdf(double x, double a, double b) {
  const boost::math::negative_binomial_distribution<double, NoThrowPolicy> d(a, b);
  return boost::math::pdf(d, x);
}

inline double negative_binomial_cdf(double x, double a, double b, bool lower_tail) {
  const boost::math::negative_binomial_distribution<double, NoThrowPolicy> d(a, b);
  return lower_tail ? boost::math::cdf(d, x)
                    : boost::math::cdf(boost::math::complement(d, x));
}

inline double negative_binomial_quantile(double x, double a, double b, bool lower_tail) {
  if (!std::isfinite(a) || a<0 || !(b>0 && b<=1)) return kNaN;
  if (a==0 || b==1) return discrete_cdf_quantile(x,0,lower_tail,
      [](double,bool lower){return lower ? 1.0 : 0.0;});
  return discrete_cdf_quantile(x,kPosInf,lower_tail,[&](double q,bool lower) {
    return negative_binomial_cdf(q,a,b,lower);
  });
}

inline double poisson_pdf(double x, double a) {
  const boost::math::poisson_distribution<double, NoThrowPolicy> d(a);
  return boost::math::pdf(d, x);
}

inline double poisson_cdf(double x, double a, bool lower_tail) {
  const boost::math::poisson_distribution<double, NoThrowPolicy> d(a);
  return lower_tail ? boost::math::cdf(d, x)
                    : boost::math::cdf(boost::math::complement(d, x));
}

inline double poisson_quantile(double x, double a, bool lower_tail) {
  if (!std::isfinite(a) || a<0) return kNaN;
  if (a==0) return discrete_cdf_quantile(x,0,lower_tail,
      [](double,bool lower){return lower ? 1.0 : 0.0;});
  return discrete_cdf_quantile(x,kPosInf,lower_tail,[&](double q,bool lower) {
    return poisson_cdf(q,a,lower);
  });
}


// Extended precision tail primitives for the package-owned transformed laws.
inline ExtendedReal normal_cdf_extended(double x, bool lower) { const boost::math::normal_distribution<ExtendedReal, NoThrowPolicy> d; return lower ? boost::math::cdf(d, static_cast<ExtendedReal>(x)) : boost::math::cdf(boost::math::complement(d, static_cast<ExtendedReal>(x))); }
inline ExtendedReal gamma_cdf_extended(double x, double a, bool lower) { const boost::math::gamma_distribution<ExtendedReal, NoThrowPolicy> d(a); return lower ? boost::math::cdf(d, static_cast<ExtendedReal>(x)) : boost::math::cdf(boost::math::complement(d, static_cast<ExtendedReal>(x))); }

// Log-tail primitives for the package-owned Fernandez--Steel families.
// Evaluate the smaller tail first, retaining complements with log1p/expm1.
inline double fs_log1mexp(double x) {
  return log1mexp(x);
}
inline double fs_normal_p(double x, bool lower, bool log_p) {
  const double a = std::fabs(x);
  const boost::math::normal_distribution<ExtendedReal, NoThrowPolicy> d;
  const ExtendedReal tail = boost::math::cdf(boost::math::complement(d, (ExtendedReal)a));
  double lp = (double)std::log(tail);
  if (tail < std::numeric_limits<ExtendedReal>::min() && std::isfinite(a)) {
    // Mills expansion: terms decrease rapidly in the underflow region.
    const double inv = 1.0/a/a;
    double sum = 1.0, term = 1.0;
    for (int k=1; k<=12; ++k) { term *= -(2*k-1)*inv; sum += term; }
    lp = -0.5*a*a - std::log(a) - 0.5*std::log(2.0*cppunivariate::pi) + std::log(sum);
  }
  if ((x < 0.0) != lower) lp = fs_log1mexp(lp);
  return log_p ? lp : std::exp(lp);
}

// Positive-root inversion for log probabilities beyond the backend exponent
// range. This fallback also avoids exp(log_p) underflow on platforms where
// ExtendedReal has the same range as double.
template <typename LogTail>
inline double fs_log_quantile(double lp, LogTail tail) {
  double hi = 1.0;
  while (tail(hi) > lp) {
    if (hi >= std::numeric_limits<double>::max()/2.0) return kPosInf;
    hi *= 2.0;
  }
  double lo = 0.0;
  for (int i=0; i<1100; ++i) {
    const double mid = lo + (hi-lo)*0.5;
    if (mid == lo || mid == hi) return mid;
    const double value=tail(mid);
    if (std::isnan(value)) return kNaN;
    if (value > lp) lo = mid; else hi = mid;
  }
  return kNaN;
}
inline double fs_normal_q(double lp, bool lower) {
  if (lp > -std::log(2.0)) return fs_normal_q(fs_log1mexp(lp), !lower);
  if (lp == kNegInf) return lower ? kNegInf : kPosInf;
  const boost::math::normal_distribution<ExtendedReal, NoThrowPolicy> d;
  const double q = lp < std::log(std::numeric_limits<ExtendedReal>::min()) + CPPUNIV_EXTENDED_LITERAL(2.0,2.0L) ? fs_log_quantile(lp, [](double x) { return fs_normal_p(x,false,true); })
      : (double)boost::math::quantile(boost::math::complement(d, std::exp((ExtendedReal)lp)));
  return lower ? -q : q;
}

// Continued fraction for the incomplete beta used only when the direct
// Student tail underflows. The logarithmic prefactor is never exponentiated.
inline double fs_beta_fraction(double a, double b, double x) {
  const double tiny = 1e-300;
  double c=1.0, d=1.0-(a+b)*x/(a+1.0);
  if (std::fabs(d)<tiny) d=tiny;
  d=1.0/d;
  double h=d;
  for (int m=1;m<=10000;++m) {
    const double m2=2.0*m;
    double aa=m*(b-m)*x/((a+m2-1.0)*(a+m2));
    d=1.0+aa*d; if(std::fabs(d)<tiny) d=tiny;
    c=1.0+aa/c; if(std::fabs(c)<tiny) c=tiny;
    d=1.0/d; h*=d*c;
    aa=-(a+m)*(a+b+m)*x/((a+m2)*(a+m2+1.0));
    d=1.0+aa*d; if(std::fabs(d)<tiny) d=tiny;
    c=1.0+aa/c; if(std::fabs(c)<tiny) c=tiny;
    d=1.0/d;
    const double change=d*c; h*=change;
    if(std::fabs(change-1.0)<4e-16) return h;
  }
  return kNaN;
}
inline double fs_t_p(double x, double nu, bool lower, bool log_p) {
  const double a = std::fabs(x);
  const boost::math::students_t_distribution<ExtendedReal, NoThrowPolicy> d(nu);
  const ExtendedReal tail = boost::math::cdf(boost::math::complement(d, (ExtendedReal)a));
  double lp = (double)std::log(tail);
  if (tail == CPPUNIV_EXTENDED_LITERAL(0.0,0.0L) && std::isfinite(a)) {
    const double logz = -std::log1p(a/std::sqrt(nu)*(a/std::sqrt(nu)));
    const double stable_logz = std::isfinite(logz) ? logz : std::log(nu)-2.0*std::log(a);
    const double z = std::exp(stable_logz);
    const double aa=nu/2.0;
    // Gamma ratio from Boost avoids cancellation at large degrees of freedom.
    const double logbeta = std::log(boost::math::tgamma_delta_ratio(aa,0.5,NoThrowPolicy{})) + 0.5*std::log(cppunivariate::pi);
    lp = -std::log(2.0) + aa*stable_logz + 0.5*std::log1p(-z)
        - logbeta + std::log(fs_beta_fraction(aa,0.5,z)/aa);
  }
  if ((x < 0.0) != lower) lp=fs_log1mexp(lp);
  return log_p ? lp : std::exp(lp);
}
inline double fs_t_q(double lp, double nu, bool lower) {
  if (lp > -std::log(2.0)) return fs_t_q(fs_log1mexp(lp), nu, !lower);
  if (lp == kNegInf) return lower ? kNegInf : kPosInf;
  const boost::math::students_t_distribution<ExtendedReal, NoThrowPolicy> d(nu);
  const double q = lp < std::log(std::numeric_limits<ExtendedReal>::min()) + CPPUNIV_EXTENDED_LITERAL(2.0,2.0L) ? fs_log_quantile(lp, [&](double x) { return fs_t_p(x,nu,false,true); })
      : (double)boost::math::quantile(boost::math::complement(d,std::exp((ExtendedReal)lp)));
  return lower ? -q : q;
}
inline double fs_gamma_p(double x, double shape, bool lower, bool log_p) {
  if (x == kPosInf) return lower ? (log_p ? 0.0 : 1.0) : (log_p ? kNegInf : 0.0);
  const boost::math::gamma_distribution<ExtendedReal, NoThrowPolicy> d(shape);
  const ExtendedReal p=boost::math::cdf(d,(ExtendedReal)x);
  const ExtendedReal q=boost::math::cdf(boost::math::complement(d,(ExtendedReal)x));
  double lp=(double)std::log(p), lq=(double)std::log(q);
  if(q<std::numeric_limits<ExtendedReal>::min() && std::isfinite(x)) {
    double sum=1.0, term=1.0;
    bool converged=false;
    for(int k=1;k<=1000000;++k) {
      term *= (shape-k)/x; sum += term;
      const double ratio=std::fabs(shape-k-1)/x;
      // Before the sign change a geometric majorant bounds the remainder;
      // afterwards the decreasing alternating expansion is bounded by its
      // first omitted term. Failure to certify is reported as NaN.
      if (ratio < 1 && std::fabs(term)*ratio *
          (k>=shape ? 1.0 : 1.0/(1.0-ratio)) < std::fabs(sum)*2e-16) {
        converged=true; break;
      }
    }
    if (!converged || !(sum>0)) return kNaN;
    lq=(shape-1.0)*std::log(x)-x-std::lgamma(shape)+std::log(sum);
  }
  if(p>CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)) lp=fs_log1mexp(lq);
  else lq=fs_log1mexp(lp);
  const double result=lower ? lp : lq;
  return log_p ? result : std::exp(result);
}
inline double fs_gamma_q(double lp, double shape, bool lower) {
  if(lp > -std::log(2.0)) return fs_gamma_q(fs_log1mexp(lp),shape,!lower);
  if(lp==kNegInf) return lower ? 0.0 : kPosInf;
  const boost::math::gamma_distribution<ExtendedReal, NoThrowPolicy> d(shape);
  if(!lower && lp < std::log(std::numeric_limits<ExtendedReal>::min()) + CPPUNIV_EXTENDED_LITERAL(2.0,2.0L))
    return fs_log_quantile(lp,[&](double x) { return fs_gamma_p(x,shape,false,true); });
  const ExtendedReal p=std::exp((ExtendedReal)lp);
  return (double)(lower ? boost::math::quantile(d,p)
      : boost::math::quantile(boost::math::complement(d,p)));
}

} // namespace special
} // namespace cppunivariate
