// Centralized special functions and dpqr primitives used by cppunivariate.
//
// Single seam between cppunivariate and the math backend: today everything
// routes through Boost.Math (shipped via the BH package on CRAN).
// Swap this file to retarget another backend.

#pragma once

#include "constants.hpp"
#include <boost/math/distributions/gamma.hpp>
#include <boost/math/distributions/logistic.hpp>
#include <boost/math/distributions/normal.hpp>
#include <boost/math/distributions/students_t.hpp>
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

inline double zeta(double s) { return boost::math::zeta(s, NoThrowPolicy{}); }

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

// --- normal density (kept inline; closed-form is fine) --------------------

inline double dnorm(double x, double mean, double sd, bool log) {
  if (!std::isfinite(sd) || sd <= 0.0) {
    return kNaN;
  }
  const double z = (x - mean) / sd;
  const double log_pref = -std::log(sd) - 0.5 * std::log(2.0 * M_PI);
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
  if (!(size > 0.0) || !(prob > 0.0) || prob > 1.0) {
    return kNaN;
  }
  if (!std::isfinite(x) || x < 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double k = std::floor(x + 0.5);
  if (std::fabs(k - x) > 1e-7) {
    return log ? kNegInf : 0.0;
  }
  if (prob == 1.0) return k == 0.0 ? (log ? 0.0 : 1.0) : (log ? kNegInf : 0.0);
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
           0.5 * std::log(df * M_PI) -
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

inline double binomial_quantile(double x, double a, double b, bool lower_tail) {
  const boost::math::binomial_distribution<double, NoThrowPolicy> d(a, b);
  return lower_tail ? boost::math::quantile(d, x)
                    : boost::math::quantile(boost::math::complement(d, x));
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
  const boost::math::negative_binomial_distribution<double, NoThrowPolicy> d(a, b);
  return lower_tail ? boost::math::quantile(d, x)
                    : boost::math::quantile(boost::math::complement(d, x));
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
  const boost::math::poisson_distribution<double, NoThrowPolicy> d(a);
  return lower_tail ? boost::math::quantile(d, x)
                    : boost::math::quantile(boost::math::complement(d, x));
}

// Extended precision tail primitives for the package-owned transformed laws.
inline long double normal_cdf_extended(double x, bool lower) { const boost::math::normal_distribution<long double, NoThrowPolicy> d; return lower ? boost::math::cdf(d, static_cast<long double>(x)) : boost::math::cdf(boost::math::complement(d, static_cast<long double>(x))); }
inline long double gamma_cdf_extended(double x, double a, bool lower) { const boost::math::gamma_distribution<long double, NoThrowPolicy> d(a); return lower ? boost::math::cdf(d, static_cast<long double>(x)) : boost::math::cdf(boost::math::complement(d, static_cast<long double>(x))); }

} // namespace special
} // namespace cppunivariate
