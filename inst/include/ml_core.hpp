// Core (non-exported) implementations for ML estimators.
// Shared by Rcpp wrappers in src/ml_exports.cpp.

#pragma once

#include "ml_common.hpp"
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <cmath>
#include <string>
#include <variant>

namespace mlcore {

// ---------- inverse digamma ----------
inline double inverse_digamma_scalar(double x) {
  constexpr double GAMMA_C   = 0.57721566490153286060;
  constexpr double ZETA3_C   = 1.2020569031595942854;
  constexpr double PI_C      = M_PI;
  constexpr double PI_SQ_C   = PI_C * PI_C;

  double y, r;
  if (x + GAMMA_C < -30.0) {
    double t     = x + GAMMA_C;
    double t_inv = 1.0 / t;
    double t3    = t_inv * t_inv * t_inv;
    y = -t_inv + (PI_SQ_C / 6.0) * t3 + (ZETA3_C / 2.0) * t3 * t_inv;
  } else {
    y = 1.0 / std::log1p(std::exp(-x));
    for (int i = 0; i < 2; ++i) {
      r = Rf_digamma(y) - x;
      y -= r / Rf_psigamma(y, 1);
    }
  }
  for (int k = 0; k < 2; ++k) {
    r = Rf_digamma(y) - x;
    if (std::fabs(r) < 1e-14) break;
    y -= r / Rf_psigamma(y, 1);
  }
  return y;
}

// ---------- Burr ----------
struct RootResult {
  double k;
  int iterations;
};

struct BurrLmomResult {
  double k;
  double c;
  int iterations;
};

struct BurrMLE {
  double shape1;
  double shape2;
  int iterations;
};

inline double logit(double p) { return std::log(p / (1.0 - p)); }

inline double logistic(double x) {
  const double e = std::exp(-x);
  return 1.0 / (1.0 + e);
}

inline double logistic_pdf(double x) {
  const double L = logistic(x);
  return L * (1.0 - L);
}

inline RootResult invert_lower_logit(double cv, double tol = kTolDefault,
                                     int maxit = kMaxIterDefault) {
  constexpr double NEULER_CST = -0.5772156649015328606;
  constexpr double BASEL_CST  = M_PI * M_PI / 6.0;
  const double loc   = -0.5495872;
  const double scl   = 0.6103216;
  const double cv_lo = 0.1;
  const double cv_hi = 0.95;
  const double eps   = 1e-15;

  double k0;
  if (cv < cv_lo) {
    k0 = 1.0 - std::sqrt(2.0 * cv / BASEL_CST);
  } else if (cv > cv_hi) {
    k0 = std::sqrt((1.0 - cv) / (cv * BASEL_CST));
  } else {
    double y    = std::min(1.0 - eps, std::max(eps, 1.0 - cv));
    double xlog = loc + scl * logit(y);
    k0          = logistic((xlog - loc) / scl);
  }
  k0 = std::min(1.0 - eps, std::max(eps, k0));

  double x   = logit(k0);
  int iter   = 0;
  for (; iter < maxit; ++iter) {
    double k     = logistic(x);
    double di    = Rf_psigamma(k, 0);
    double tri   = Rf_psigamma(k, 1);
    double tetra = Rf_psigamma(k, 2);

    double pd  = di - NEULER_CST;
    double bpt = BASEL_CST + tri;
    double f   = (pd * pd) / bpt - cv;

    double fpk = ((2.0 * pd * tri * bpt) - (pd * pd * tetra)) / (bpt * bpt);
    double df  = fpk * logistic_pdf(x);

    double step = f / df;
    double x1   = x - step;
    if (std::abs(x1 - x) < (std::abs(x) * tol + 1e-3 * tol)) {
      x = x1;
      break;
    }
    x = x1;
  }

  return {logistic(x), iter + 1};
}

inline RootResult invert_lower_log(double cv, double tol = kTolDefault,
                                   int maxit = kMaxIterDefault) {
  constexpr double NEULER_CST = -0.5772156649015328606;
  constexpr double BASEL_CST  = M_PI * M_PI / 6.0;
  const double loc   = -0.5495872;
  const double scl   = 0.6103216;
  const double cv_lo = 0.1;
  const double cv_hi = 0.95;
  const double eps   = 1e-15;

  double k0;
  if (cv < cv_lo) {
    k0 = 1.0 - std::sqrt(2.0 * cv / BASEL_CST);
  } else if (cv > cv_hi) {
    k0 = std::sqrt((1.0 - cv) / (cv * BASEL_CST));
  } else {
    double y    = std::min(1.0 - eps, std::max(eps, 1.0 - cv));
    double xlog = loc + scl * logit(y);
    k0          = logistic((xlog - loc) / scl);
  }
  k0 = std::max(eps, std::min(1.0 - eps, k0));

  double p   = std::log(k0);
  int iter   = 0;
  for (; iter < maxit; ++iter) {
    double k     = std::exp(p);
    double di    = Rf_psigamma(k, 0);
    double tri   = Rf_psigamma(k, 1);
    double tetra = Rf_psigamma(k, 2);

    double pd  = di - NEULER_CST;
    double bpt = BASEL_CST + tri;
    double f   = (pd * pd) / bpt - cv;

    double fpk = ((2.0 * pd * tri * bpt) - (pd * pd * tetra)) / (bpt * bpt);
    double dfp = fpk * k;

    double step = f / dfp;
    double p1   = p - step;
    if (std::abs(p1 - p) < (std::abs(p) * tol + 1e-3 * tol)) {
      p = p1;
      break;
    }
    p = p1;
  }

  return {std::exp(p), iter + 1};
}

inline RootResult invert_upper(double cv, double tol = kTolDefault,
                               int maxit = kMaxIterDefault) {
  constexpr double NEULER_CST = -0.5772156649015328606;
  constexpr double BASEL_CST  = M_PI * M_PI / 6.0;
  const double thr = 0.2;
  const double eps = 1e-9;

  double k0;
  if (cv < thr) {
    k0 = 1.0 + std::sqrt(2.0 * cv / BASEL_CST);
  } else {
    k0 = std::exp(std::sqrt(cv * BASEL_CST));
  }
  k0 = std::max(1.0 + eps, k0);

  double x   = std::log(k0 - 1.0);
  int iter   = 0;
  for (; iter < maxit; ++iter) {
    double kk    = std::exp(x) + 1.0;
    double di    = Rf_psigamma(kk, 0);
    double tri   = Rf_psigamma(kk, 1);
    double tetra = Rf_psigamma(kk, 2);

    double pd  = di - NEULER_CST;
    double bpt = BASEL_CST + tri;
    double f   = (pd * pd) / bpt - cv;

    double fpk = ((2.0 * pd * tri * bpt) - (pd * pd * tetra)) / (bpt * bpt);
    double df  = fpk * std::exp(x);

    double step = f / df;
    double x1   = x - step;
    if (std::abs(x1 - x) < (std::abs(x) * tol + 1e-3 * tol)) {
      x = x1;
      break;
    }
    x = x1;
  }

  return {std::exp(x) + 1.0, iter + 1};
}

inline BurrLmomResult burr_lmom_core(double mu, double sigma2,
                                     bool use_logit_lower = true) {
  constexpr double BASEL_CST = M_PI * M_PI / 6.0;
  double k_hat = NA_REAL;
  double c_hat = NA_REAL;
  int iter     = 0;

  const double eps = kEps;

  if (sigma2 > 0.0 && std::abs(mu) >= eps) {
    double cv = (mu * mu) / sigma2;
    RootResult root = (mu > 0.0)
      ? (use_logit_lower ? invert_lower_logit(cv) : invert_lower_log(cv))
      : invert_upper(cv);
    k_hat = root.k;
    iter  = root.iterations;

    double t1  = Rf_psigamma(k_hat, 1);
    double csq = (BASEL_CST + t1) / sigma2;
    if (csq > 0.0) {
      c_hat = std::sqrt(csq);
    }
  } else if (std::abs(mu) < eps) {
    k_hat = 1.0;
    double t1  = Rf_psigamma(1.0, 1);
    double csq = (BASEL_CST + t1) / sigma2;
    if (csq > 0.0) {
      c_hat = std::sqrt(csq);
    }
  }

  return {k_hat, c_hat, iter};
}

inline BurrMLE burr_mle_core(const arma::vec &x, double c_start, double tol,
                             int maxit) {
  const int n      = static_cast<int>(x.n_elem);
  const double nd  = static_cast<double>(n);
  arma::vec log_x  = arma::log(x);
  const double *lxp = log_x.memptr();
  const double sum_l = arma::sum(log_x);

  double logc = std::log(c_start);
  int iter    = 0;
  for (; iter < maxit; ++iter) {
    double c = std::exp(logc);

    double g = 0.0, dg = 0.0, d2g1 = 0.0, d2g2 = 0.0;
    for (int i = 0; i < n; ++i) {
      double lx = lxp[i];
      double xc = std::exp(c * lx);
      double t  = xc / (xc + 1.0);
      g    += std::log1p(xc);
      dg   += lx * t;
      double lx_t = lx * t;
      d2g1 += lx * lx * t;
      d2g2 += lx_t * lx_t;
    }
    double d2g = d2g1 - d2g2;
    double g2  = g * g;

    double f_c  = nd / c - nd * dg / g + sum_l - dg;
    double df_c = -nd / (c * c) - nd * (g * d2g - dg * dg) / g2 - d2g;

    double f_p  = c * f_c;
    double d2_p = c * (f_c + c * df_c);

    double step_p = f_p / d2_p;
    logc -= step_p;
    if (std::fabs(step_p) < tol) {
      ++iter;
      break;
    }
  }

  double c = std::exp(logc);
  arma::vec xc_final = arma::exp(c * log_x);
  double g_final     = arma::sum(arma::log1p(xc_final));
  double k           = nd / g_final;

  return {k, c, (iter == maxit ? maxit : iter)};
}

// ---------- Cauchy ----------
struct CauchyResult {
  double mu;
  double scale;
  int iterations;
};

inline CauchyResult fit_cauchy(const arma::vec &x, double tol, int maxit) {
  int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, 0}; // wrapper will stop

  std::vector<double> data(x.begin(), x.end());
  std::nth_element(data.begin(), data.begin() + n / 2, data.end());
  double mu = data[n / 2];

  for (int i = 0; i < n; ++i) {
    data[i] = std::fabs(data[i] - mu);
  }
  std::nth_element(data.begin(), data.begin() + n / 2, data.end());
  double theta = std::log(data[n / 2] + kEps);

  double dmu = 0.0, dth = 0.0;

  int iter;
  for (iter = 1; iter <= maxit; ++iter) {
    const double inv_s  = std::exp(-theta);
    const double inv_s2 = inv_s * inv_s;

    double sum1 = 0.0, sum2 = 0.0, sum3 = 0.0, sum4 = 0.0, sum5 = 0.0;
    for (int i = 0; i < n; ++i) {
      double z  = (x[i] - mu) * inv_s;
      double z2 = z * z;
      double d1 = 1.0 + z2;
      double i1 = 1.0 / d1;
      double i2 = i1 * i1;

      sum1 += z * i1;
      sum2 += (z2 - 1) * i1;
      sum3 += (1 - z2) * i2;
      sum4 +=   z    * i2;
      sum5 +=   z2   * i2;
    }

    double U_mu    = 2 * inv_s  * sum1;
    double U_th    =       1.0 * sum2;
    double H_mm    = 2 * inv_s2 * sum3;
    double H_mth   = 4 * inv_s  * sum4;
    double H_thth  = 4           * sum5;

    double det = H_mm * H_thth - H_mth * H_mth;
    dmu  = ( H_thth * U_mu   - H_mth * U_th) / det;
    dth  = (-H_mth   * U_mu   + H_mm   * U_th) / det;

    mu    += dmu;
    theta += dth;

    if (std::max(std::fabs(dmu), std::fabs(dth)) < tol) break;
  }

  return CauchyResult{mu, std::exp(theta), iter};
}

// ---------- Gumbel ----------
struct GumbelResult {
  double mu;
  double sigma;
  int iterations;
};

inline GumbelResult fit_gumbel_newton(const arma::vec &x, double tol, int maxit) {
  const int n = static_cast<int>(x.n_elem);
  const double* xp = x.memptr();

  double sum_x = 0.0;
  double sum_x2 = 0.0;
  for (double xi : x) {
    sum_x  += xi;
    sum_x2 += xi * xi;
  }
  const double xbar = sum_x / n;
  const double var0 = sum_x2 / n - xbar * xbar;

  double theta = std::log(std::sqrt(var0) * 2.45 / M_PI);
  int iter = 0;

  for (iter = 1; iter <= maxit; ++iter) {
    const double sigma = std::exp(theta);
    const double invs = 1.0 / sigma;
    const double negs = -invs;

    double psi0 = 0.0, psi1 = 0.0, psi2 = 0.0;
    for (int i = 0; i < n; ++i) {
      const double v = xp[i] - xbar;
      const double e = std::exp(v * negs);
      psi0 += e;
      psi1 += v * e;
      psi2 += v * v * e;
    }
    psi1 /= psi0;
    psi2  = psi2 / psi0 - psi1 * psi1;

    const double f   = sigma + psi1;
    const double df  = 1.0 + psi2 * (invs * invs);
    const double dth = f / (df * sigma);

    theta -= dth;
    if (std::abs(dth) < std::abs(theta) * tol) break;
  }

  const double sigma = std::exp(theta);
  const double invsF = -1.0 / sigma;
  double sum_e = 0.0;
  for (int i = 0; i < n; ++i) {
    sum_e += std::exp((xp[i] - xbar) * invsF);
  }
  const double mu = -sigma * std::log(sum_e / n) + xbar;

  return GumbelResult{mu, sigma, iter};
}

// ---------- Logistic ----------
struct LogisticResult {
  double mu;
  double scale;
  int iterations;
};

inline LogisticResult logistic_mle(const arma::vec& x,
                            double tol,
                            int maxit) {
  const int n = static_cast<int>(x.n_elem);

  // One-pass mean/variance (Welford)
  double mean = 0.0;
  double M2   = 0.0;
  for (int i = 0; i < n; ++i) {
    double diff = x[i] - mean;
    mean += diff / static_cast<double>(i + 1);
    M2   += diff * (x[i] - mean);
  }

  double mu = mean;
  double sample_var = (n > 1) ? M2 / static_cast<double>(n - 1) : 0.0;
  if (sample_var <= 0.0) {
    sample_var = 1.0;
  }
  double theta = std::log(std::sqrt(sample_var) * std::sqrt(3.0) / M_PI);

  for (int iter = 0; iter < maxit; ++iter) {
    const double inv_s  = std::exp(-theta);
    const double inv_s2 = inv_s * inv_s;

    double U1 = 0.0, U2 = 0.0;
    double sum_c2 = 0.0, sum_tz = 0.0, sum_z2c2 = 0.0;

    for (double xi : x) {
      double z  = (xi - mu) * inv_s;
      double h  = 0.5 * z;
      double th = std::tanh(h);
      double c2 = 1.0 - th * th;

      U1       += th;
      U2       += z * th;
      sum_c2   += c2;
      sum_tz   += th + 0.5 * z * c2;
      sum_z2c2 += z * z * c2;
    }

    U1 *= inv_s;
    U2 -= n;

    double J11 = 0.5 * sum_c2 * inv_s2;
    double J12 = inv_s * sum_tz;
    double J22 = (U2 + n) + 0.5 * sum_z2c2;

    double det = J11 * J22 - J12 * J12;
    double dmu    = ( J22 * U1 - J12 * U2) / det;
    double dtheta = (-J12 * U1 + J11 * U2) / det;

    mu    += dmu;
    theta += dtheta;

    if (std::max(std::fabs(dmu), std::fabs(dtheta)) < tol) {
      return {mu, std::exp(theta), iter + 1};
    }
  }

  return {mu, std::exp(theta), maxit};
}

// ---------- Beta ----------
struct BetaResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
};

inline BetaResult beta_mle(const arma::vec& x, double tol, int maxit) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  double s = arma::sum(arma::log(x)) / n;
  double r = arma::sum(arma::log(1.0 - x)) / n;
  double m = arma::mean(x);
  double v = arma::mean(arma::square(x)) - m * m;

  double nu = m * (1.0 - m) / v - 1.0;
  double b  = std::log((1.0 - m) * nu);

  int iter = 0;
  double alpha = NA_REAL;
  for (iter = 1; iter <= maxit; ++iter) {
    double exp_b      = std::exp(b);
    double digamma_b  = Rf_digamma(exp_b);
    double trigamma_b = Rf_psigamma(exp_b, 1);

    double y = s + digamma_b - r;

    // invert digamma for a with a few NR steps
    alpha = 1.0 / std::log1p(std::exp(-y));
    for (int k = 0; k < 3; ++k) {
      double ra = Rf_digamma(alpha) - y;
      alpha    -= ra / Rf_psigamma(alpha, 1);
    }

    double f  = exp_b * (digamma_b - r - Rf_digamma(alpha + exp_b));
    double df = f + exp_b * exp_b *
      (trigamma_b - Rf_psigamma(alpha + exp_b, 1) * (trigamma_b / Rf_psigamma(alpha,1) + 1.0));

    double b0 = b - f / df;
    if (std::abs((b0 - b) / b0) < tol) {
      b = b0;
      break;
    }
    b = b0;
  }

  double beta = std::exp(b);
  double logLik = n * ((alpha - 1.0) * s + (beta - 1.0) * r - std::lgamma(alpha) - std::lgamma(beta) + std::lgamma(alpha + beta));
  return {alpha, beta, logLik, iter};
}

// ---------- Gamma ----------
struct GammaResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
};

inline GammaResult gamma_mle(const arma::vec& x, double tol, int maxit) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  double mean_hat = arma::mean(x);
  double lx_bar   = arma::mean(arma::log(x));
  double s        = std::log(mean_hat) - lx_bar;

  auto f_over_df = [s](double shape0) {
    return (std::log(shape0) - Rf_digamma(shape0) - s) /
           (1.0 / shape0 - Rf_psigamma(shape0, 1));
  };

  double shape0 = 1.0 / (12.0 * s) * (3.0 - s + std::sqrt((s - 3.0) * (s - 3.0) + 24.0 * s)) * 0.9989 + 0.0102;
  double shape  = shape0;
  int iter = 0;
  for (iter = 1; iter <= maxit; ++iter) {
    double step = f_over_df(shape);
    shape      -= step;
    if (std::abs(step) < tol) break;
  }
  double rate = shape / mean_hat;
  double logLik = n * (shape * std::log(rate) - std::lgamma(shape) + (shape - 1.0) * lx_bar - rate * mean_hat);
  return {shape, rate, logLik, iter};
}

// ---------- Simple closed-form ----------
struct ClosedResult {
  arma::vec estimates;
  double logLik;
};

inline ClosedResult exp_mle(const arma::vec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {arma::vec(), NA_REAL};
  double lambda = 1.0 / arma::mean(x);
  double logLik = n * std::log(lambda) - lambda * arma::accu(x);
  return {arma::vec({lambda}), logLik};
}

inline ClosedResult unif_mle(const arma::vec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {arma::vec(), NA_REAL};
  double a = x.min();
  double b = x.max();
  if (b <= a) return {arma::vec(), NA_REAL};
  double logLik = -n * std::log(b - a);
  return {arma::vec({a, b}), logLik};
}

inline ClosedResult pois_mle(const arma::ivec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {arma::vec(), NA_REAL};
  double lambda = arma::mean(arma::conv_to<arma::vec>::from(x));
  double logLik = 0.0;
  for (int i = 0; i < n; ++i) {
    logLik += Rf_dpois(static_cast<double>(x[i]), lambda, 1);
  }
  return {arma::vec({lambda}), logLik};
}

inline ClosedResult norm_mle(const arma::vec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {arma::vec(), NA_REAL};
  double mu = arma::mean(x);
  double var = arma::accu(arma::square(x - mu)) / n;
  double sigma = std::sqrt(var);
  double logLik = -0.5 * n * (std::log(2 * M_PI * var) + 1.0);
  return {arma::vec({mu, sigma}), logLik};
}

inline ClosedResult laplace_mle(const arma::vec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {arma::vec(), NA_REAL};
  arma::vec sorted = arma::sort(x);
  double mu = sorted[n/2];
  double sigma = arma::mean(arma::abs(x - mu));
  double logLik = -n * (1.0 + std::log(2.0 * sigma));
  return {arma::vec({mu, sigma}), logLik};
}

inline ClosedResult geom_mle(const arma::ivec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {arma::vec(), NA_REAL};
  double prob = 1.0 / (arma::mean(arma::conv_to<arma::vec>::from(x)) + 1.0);
  double logLik = 0.0;
  for (int i = 0; i < n; ++i) {
    logLik += Rf_dgeom(static_cast<double>(x[i]), prob, 1);
  }
  return {arma::vec({prob}), logLik};
}

inline ClosedResult pareto_mle(const arma::vec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {arma::vec(), NA_REAL};
  double xmin = x.min();
  if (xmin <= 0.0) return {arma::vec(), NA_REAL};
  double alpha = n / arma::accu(arma::log(x / xmin));
  double logLik = n * std::log(alpha) + n * alpha * std::log(xmin) - (alpha + 1.0) * arma::accu(arma::log(x));
  return {arma::vec({alpha, xmin}), logLik};
}

inline ClosedResult rayleigh_mle(const arma::vec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {arma::vec(), NA_REAL};
  double sigma2 = arma::mean(arma::square(x)) / 2.0;
  if (sigma2 <= 0.0) return {arma::vec(), NA_REAL};
  double sigma = std::sqrt(sigma2);
  double logLik = arma::accu(arma::log(x)) - 2.0 * n * std::log(sigma) - arma::accu(arma::square(x)) / (2.0 * sigma2);
  return {arma::vec({sigma}), logLik};
}

// Binomial (Newton on size)
struct BinomResult {
  double size;
  double prob;
  double logLik;
  int iterations;
};

inline BinomResult binom_mle(const arma::ivec& x, double tol, int maxit) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};
  double x_bar = arma::mean(arma::conv_to<arma::vec>::from(x));
  double var   = arma::var(arma::conv_to<arma::vec>::from(x), 1); // population var
  if (x_bar / var <= 1.0) {
    return {NA_REAL, NA_REAL, NA_REAL, 0};
  }

  int max_x = x.max();
  // counts
  arma::ivec uniq = arma::unique(x);
  arma::ivec counts(uniq.n_elem, arma::fill::zeros);
  for (arma::uword i = 0; i < uniq.n_elem; ++i) {
    counts[i] = static_cast<int>(arma::accu(x == uniq[i]));
  }

  auto log_likelihood = [&](double size) {
    double prob = x_bar / size;
    double ll = n * x_bar * std::log(prob) + n * (size - x_bar) * std::log(1.0 - prob);
    for (int i = 0; i < uniq.n_elem; ++i) {
      ll += counts[i] * std::lgamma(size + 1.0)
        - counts[i] * std::lgamma(static_cast<double>(uniq[i]) + 1.0)
        - counts[i] * std::lgamma(size - static_cast<double>(uniq[i]) + 1.0);
    }
    return ll;
  };

  auto f_over_df = [&](double size) {
    double size_m_x = size - x_bar;
    double f = n * (std::log(size_m_x) - std::log(size))
      + n * Rf_digamma(size + 1.0);
    double df = n * x_bar / (size * size_m_x)
      + n * Rf_psigamma(size + 1.0, 1);
    for (arma::uword i = 0; i < uniq.n_elem; ++i) {
      double arg = size - static_cast<double>(uniq[i]) + 1.0;
      f  -= counts[i] * Rf_digamma(arg);
      df -= counts[i] * Rf_psigamma(arg, 1);
    }

    return f / df;
  };

  double size0 = static_cast<double>(max_x);
  double size = size0;
  for (int i = 1; i <= maxit; ++i) {
    double step = f_over_df(size);
    size -= step;
    if (std::abs(step) < tol) {
      double prob = x_bar / size;
      double ll = log_likelihood(size);
      return {size, prob, ll, i};
    }
  }
  double prob = x_bar / size;
  double ll = log_likelihood(size);
  return {size, prob, ll, maxit};
}

inline ClosedResult lognormal_mle(const arma::vec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {arma::vec(), NA_REAL};
  arma::vec y = arma::log(x);
  double meanlog = arma::mean(y);
  double sdlog   = std::sqrt(arma::var(y, 1));
  double logLik  = -0.5 * n * (std::log(2 * M_PI * sdlog * sdlog) + 1.0)
    - arma::accu(arma::log(x));
  return {arma::vec({meanlog, sdlog}), logLik};
}

inline ClosedResult logitnorm_mle(const arma::vec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {arma::vec(), NA_REAL};
  arma::vec y = arma::log(x / (1.0 - x));
  double mu = arma::mean(y);
  double sigma = std::sqrt(arma::var(y, 1));
  double logLik = -0.5 * n * (std::log(2 * M_PI * sigma * sigma) + 1.0)
    - arma::accu(arma::log(x % (1.0 - x)));
  return {arma::vec({mu, sigma}), logLik};
}

// ---------- Weibull (via Gumbel transform) ----------
struct WeibullResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
};

inline WeibullResult weibull_mle(const arma::vec& x,
                                 double tol = kTolDefault,
                                 int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  arma::vec log_x = -arma::log(x);
  const double s  = -arma::accu(log_x) / static_cast<double>(n);
  arma::vec shifted = log_x + s;

  GumbelResult g = fit_gumbel_newton(shifted, tol, maxit);
  const double shape = 1.0 / g.sigma;
  const double scale = std::exp(-g.mu + s);
  const double logLik = static_cast<double>(n) *
    (std::log(shape / scale) + (shape - 1.0) * (s - std::log(scale)) - 1.0);

  return {shape, scale, logLik, g.iterations};
}

// ---------- Inverse Weibull (via Gumbel transform) ----------
struct InvWeibullResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
};

inline InvWeibullResult invweibull_mle(const arma::vec& x,
                                       double tol = kTolDefault,
                                       int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  arma::vec log_x = arma::log(x);
  const double s  = -arma::accu(log_x) / static_cast<double>(n);
  arma::vec shifted = log_x + s;

  GumbelResult g = fit_gumbel_newton(shifted, tol, maxit);
  const double rate_log = -g.mu + s;
  const double rate     = std::exp(rate_log);
  const double shape    = 1.0 / g.sigma;
  const double logLik   = static_cast<double>(n) *
    (std::log(shape) + shape * (-rate_log + s) - 1.0 + s);

  return {shape, rate, logLik, g.iterations};
}

// ---------- Inverse Gamma ----------
struct InvGammaResult {
  double shape;
  double rate;
  double logLik;
  int iterations;
};

inline InvGammaResult invgamma_mle(const arma::vec& x,
                                   double tol = kTolDefault,
                                   int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  arma::vec invx = 1.0 / x;
  GammaResult g  = gamma_mle(invx, tol, maxit);

  const double L = arma::mean(arma::log(x));
  const double M = arma::mean(invx);
  const double logLik = static_cast<double>(n) *
    (g.shape * std::log(g.rate) - std::lgamma(g.shape) -
     (g.shape + 1.0) * L - g.rate * M);

  return {g.shape, g.rate, logLik, g.iterations};
}

// ---------- Inverse Gaussian (closed form) ----------
struct InvGaussResult {
  double mu;
  double lambda;
  double logLik;
};

inline InvGaussResult invgauss_mle(const arma::vec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL};

  const double mu     = arma::mean(x);
  const double invx   = arma::mean(1.0 / x);
  const double lambda = 1.0 / (invx - 1.0 / mu);
  const double L      = arma::mean(arma::log(x));
  const double S      = arma::mean(arma::square(x - mu) / x);
  const double logLik = -0.5 * static_cast<double>(n) *
    (3.0 * L - std::log(lambda) + std::log(2.0 * M_PI) + lambda / (mu * mu) * S);

  return {mu, lambda, logLik};
}

// ---------- Lomax ----------
struct LomaxResult {
  double lambda;
  double kappa;
  double logLik;
  int iterations;
};

inline LomaxResult lomax_mle(const arma::vec& x,
                             double tol = kTolDefault,
                             int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  const double m   = arma::mean(x);
  const double s   = arma::mean(arma::square(x));
  const double m2  = m * m;
  if (s < 2.0 * m2) {
    return {NA_REAL, NA_REAL, NA_REAL, 0};
  }

  const double alpha0  = 2.0 * (s - m2) / (s - 2.0 * m2);
  double lambda = 1.0 / (m * (alpha0 - 1.0));
  if (lambda <= 0.0 || !std::isfinite(lambda)) {
    return {NA_REAL, NA_REAL, NA_REAL, 0};
  }

  int iter = 0;
  for (iter = 1; iter <= maxit; ++iter) {
    arma::vec lambda_x1p = 1.0 + lambda * x;
    arma::vec x_div      = x / lambda_x1p;

    const double S  = arma::mean(arma::log(lambda_x1p));
    const double S1 = arma::mean(x_div);
    const double S2 = arma::mean(arma::square(x_div));

    const double psi = S1 / S;
    const double f   = 1.0 / lambda - psi - S1;
    const double df  = psi * psi + S2 / S + S2 - 1.0 / (lambda * lambda);

    const double step   = f / df;
    const double lambda_new = lambda - step;
    if (lambda_new <= 0.0 || !std::isfinite(lambda_new)) {
      return {NA_REAL, NA_REAL, NA_REAL, iter};
    }

    if (std::abs(step) < (std::abs(lambda) * tol + tol)) {
      lambda = lambda_new;
      break;
    }
    lambda = lambda_new;
  }

  const double kappa = static_cast<double>(n) /
    arma::accu(arma::log1p(lambda * x));
  const double logLik = static_cast<double>(n) *
    (std::log(kappa) + std::log(lambda) - 1.0 - 1.0 / kappa);

  return {lambda, kappa, logLik, iter};
}

// ---------- Zero-inflated Poisson ----------
struct ZipResult {
  double lambda;
  double pi;
  double logLik;
};

inline double lambertw0(double z, double tol = 1e-12, int maxit = 30) {
  // Principal branch, z in [-1/e, 0]
  if (z == 0.0) return 0.0;
  double w;
  if (std::abs(z) < 1e-3) {
    // series approximation
    w = z - z * z + 1.5 * z * z * z;
  } else {
    w = std::log(-z);
  }
  for (int i = 0; i < maxit; ++i) {
    const double e = std::exp(w);
    const double f = w * e - z;
    const double p = w + 1.0;
    const double t = (p > 0.0) ? (f / (e * p)) : f / (e * p);
    const double step = t * (1.0 + 0.5 * t * (w + 2.0) / p);
    w -= step;
    if (std::abs(step) < (std::abs(w) * tol + tol)) break;
  }
  return w;
}

inline ZipResult zip_mle(const arma::ivec& x) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL};

  const double r0     = arma::mean(arma::conv_to<arma::vec>::from(x == 0));
  const double x_bar  = arma::mean(arma::conv_to<arma::vec>::from(x));

  double lambda = 0.0;
  double pi     = 0.0;

  if (r0 == 1.0) {
    lambda = 1.0;
    pi     = 1.0;
  } else if (r0 > std::exp(-x_bar)) {
    const double gamma = x_bar / (1.0 - r0);
    const double z     = -gamma * std::exp(-gamma);
    const double w     = lambertw0(z);
    lambda = w + gamma;
    pi     = (r0 - std::exp(-lambda)) / (1.0 - std::exp(-lambda));
  } else {
    pi     = 0.0;
    lambda = x_bar;
  }

  double logLik = 0.0;
  for (int i = 0; i < n; ++i) {
    const int xi = x[i];
    if (xi == 0) {
      logLik += std::log(pi + (1.0 - pi) * std::exp(-lambda));
    } else {
      logLik += std::log(1.0 - pi) + xi * std::log(lambda) - lambda - std::lgamma(xi + 1.0);
    }
  }

  return {lambda, pi, logLik};
}

// ---------- Zipf ----------
struct ZipfResult {
  double N;
  double shape;
  double logLik;
  int iterations;
};

inline ZipfResult zipf_mle(const arma::ivec& x,
                           double tol = kTolDefault,
                           int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  const int N = x.max();
  arma::vec seq = arma::regspace<arma::vec>(1, N);
  arma::vec log_seq = arma::log(seq);
  arma::vec log_seq2 = arma::square(log_seq);
  arma::vec div_seq = 1.0 / seq;

  const double sum_lx = arma::accu(arma::log(arma::conv_to<arma::vec>::from(x)));
  if (sum_lx == 0.0) {
    return {static_cast<double>(N), 1.0, 0.0, 0};
  }

  auto f_over_df = [&](double shape0) {
    arma::vec pow = arma::pow(div_seq, shape0);
    const double sum_shape   = arma::accu(pow);
    const double sum_log_hs  = arma::dot(pow, log_seq);
    const double sum_log2_hs = arma::dot(pow, log_seq2);
    const double top    = sum_log_hs / sum_shape;
    const double bottom = -static_cast<double>(n) * sum_log2_hs / sum_shape +
                          static_cast<double>(n) * top * top;
    return (static_cast<double>(n) * top - sum_lx) / bottom;
  };

  double shape = 1.0;
  int iter = 0;
  for (iter = 1; iter <= maxit; ++iter) {
    double step = f_over_df(shape);
    shape -= step;
    if (shape < 0.0 || !std::isfinite(shape)) break;
    if (std::abs(step) < (std::abs(shape) * tol + tol)) break;
  }
  if (shape < 0.0) {
    return {static_cast<double>(N), NA_REAL, NA_REAL, iter};
  }

  arma::vec pow = arma::pow(div_seq, shape);
  const double logLik = -shape * sum_lx - static_cast<double>(n) * std::log(arma::accu(pow));
  return {static_cast<double>(N), shape, logLik, iter};
}

// ---------- Negative binomial ----------
struct NbinomResult {
  double size;
  double prob;
  double logLik;
  int iterations;
};

inline NbinomResult nbinom_mle(const arma::ivec& x,
                               double tol = kTolDefault,
                               int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  const double x_bar = arma::mean(arma::conv_to<arma::vec>::from(x));
  if (x_bar == 0.0) {
    return {1.0, 0.5, 0.0, 0};
  }

  const double var_hat = arma::var(arma::conv_to<arma::vec>::from(x), 1);
  if (var_hat < x_bar) {
    return {NA_REAL, NA_REAL, NA_REAL, 0};
  }

  arma::ivec uniq = arma::unique(x);
  arma::ivec counts(uniq.n_elem, arma::fill::zeros);
  for (arma::uword i = 0; i < uniq.n_elem; ++i) {
    counts[i] = arma::accu(x == uniq[i]);
  }

  auto log_likelihood = [&](double size) {
    double prob = size / (x_bar + size);
    double ll = 0.0;
    for (arma::uword i = 0; i < uniq.n_elem; ++i) {
      double xi = static_cast<double>(uniq[i]);
      ll += counts[i] * (Rf_lgammafn(xi + size) - Rf_lgammafn(xi + 1.0));
    }
    ll -= static_cast<double>(n) * Rf_lgammafn(size);
    ll += static_cast<double>(n) * size * std::log(prob);
    ll += static_cast<double>(n) * std::log(1.0 - prob) * x_bar;
    return ll;
  };

  auto f_over_df = [&](double size) {
    const double prob  = size / (x_bar + size);
    const double dprob = x_bar / (size * size + x_bar * size);

    double f  = 0.0;
    double df = 0.0;
    for (arma::uword i = 0; i < uniq.n_elem; ++i) {
      double arg = size + static_cast<double>(uniq[i]);
      f  += counts[i] * Rf_digamma(arg);
      df += counts[i] * Rf_psigamma(arg, 1);
    }
    f  -= static_cast<double>(n) * Rf_digamma(size);
    df -= static_cast<double>(n) * Rf_psigamma(size, 1);
    f  += static_cast<double>(n) * std::log(prob);
    df += static_cast<double>(n) * dprob;
    return f / df;
  };

  double size = (x_bar * x_bar) / (var_hat - x_bar);
  int iter = 0;
  for (iter = 1; iter <= maxit; ++iter) {
    double step = f_over_df(size);
    size -= step;
    if (size <= 0.0 || !std::isfinite(size)) {
      return {NA_REAL, NA_REAL, NA_REAL, iter};
    }
    if (std::abs(step) < (std::abs(size) * tol + tol)) break;
  }

  const double prob = size / (x_bar + size);
  const double ll   = log_likelihood(size);
  return {size, prob, ll, iter};
}

// ---------- Nakagami ----------
struct NakaResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
};

inline NakaResult naka_mle(const arma::vec& x,
                           double tol = kTolDefault,
                           int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};
  arma::vec x2 = arma::square(x);
  GammaResult g = gamma_mle(x2, tol, maxit);
  double shape = g.shape;
  double scale = (shape / g.rate);

  const double logLik =
    static_cast<double>(n) * (shape * std::log(shape) + std::log(2.0) -
    std::lgamma(shape) - shape * std::log(scale)) +
    (2.0 * shape - 1.0) * arma::accu(arma::log(x)) -
    (shape / scale) * arma::accu(x2);

  return {shape, scale, logLik, g.iterations};
}

// ---------- Gompertz ----------
struct GompertzResult {
  double a;
  double b;
  double logLik;
  int iterations;
};

inline GompertzResult gompertz_mle(const arma::vec& x,
                                   double tol = kTolDefault,
                                   int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  const double x_sum = arma::accu(x);
  const double x_var = arma::var(x, 1);
  if (x_sum / static_cast<double>(n) < std::sqrt(x_var)) {
    return {NA_REAL, NA_REAL, NA_REAL, 0};
  }

  auto f_over_df = [&](double b) {
    arma::vec exp_bx = arma::exp(b * x);
    const double sum_bx   = arma::accu(exp_bx);
    const double sum_bx_x = arma::dot(x, exp_bx);
    const double sum_bx_x2 = arma::dot(arma::square(x), exp_bx);

    const double s  = sum_bx - static_cast<double>(n);
    const double r  = b * sum_bx_x - s;

    const double a  = b * static_cast<double>(n) / s;
    const double da = -static_cast<double>(n) * r / (s * s);
    const double d2a = static_cast<double>(n) / (s * s) *
      (b * (2.0 * sum_bx_x * sum_bx_x / s - sum_bx_x2) - 2.0 * sum_bx_x);

    const double f  = static_cast<double>(n) * da / a + x_sum;
    const double df = static_cast<double>(n) * (a * d2a - da * da) / (a * a);
    return f / df;
  };

  double b = 1e-1;
  int iter = 0;
  for (iter = 1; iter <= maxit; ++iter) {
    double step = f_over_df(b);
    b -= step;
    if (b <= 0.0 || !std::isfinite(b)) {
      return {NA_REAL, NA_REAL, NA_REAL, iter};
    }
    if (std::abs(step) < (std::abs(b) * tol + tol)) break;
  }

  arma::vec exp_bx = arma::exp(b * x);
  const double a = b * static_cast<double>(n) / (arma::accu(exp_bx) - static_cast<double>(n));
  const double logLik = static_cast<double>(n) * std::log(a) +
    b * arma::accu(x) -
    (a / b) * (arma::accu(exp_bx) - static_cast<double>(n));
  return {a, b, logLik, iter};
}

// ---------- Kumaraswamy ----------
struct KumarResult {
  double a;
  double b;
  double logLik;
  int iterations;
};

inline KumarResult kumar_mle(const arma::vec& x,
                             double tol = kTolDefault,
                             int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  const double mu = arma::mean(arma::log(x));
  const double sigma2 = arma::mean(arma::square(arma::log(x))) - mu * mu;

  auto f_over_df_lmom = [&](double a0) {
    const double b = inverse_digamma_scalar(-0.5772156649015328606 - a0 * mu);
    const double tri = Rf_psigamma(b, 1);
    const double tetra = Rf_psigamma(b, 2);
    const double basel = M_PI * M_PI / 6.0;
    const double f = a0 * tri * (basel - tri - a0 * a0 * sigma2);
    const double df = a0 * tetra * mu - 2.0 * tri * (basel + tri);
    return f / df;
  };

  double a0 = std::sqrt((M_PI * M_PI / 6.0) / sigma2);
  for (int i = 0; i < maxit; ++i) {
    const double step = f_over_df_lmom(a0);
    a0 -= step;
    if (std::abs(step) < (std::abs(a0) * tol + tol)) break;
  }
  if (a0 <= 0.0) a0 = std::sqrt((M_PI * M_PI / 6.0) / sigma2);

  auto f_over_df = [&](double a) {
    arma::vec xa = arma::exp(a * arma::log(x));
    arma::vec z  = arma::log(x) / (1.0 - xa);

    const double alpha = arma::mean(arma::log1p(-xa));
    const double beta  = arma::mean(xa % z);
    const double gamma = arma::mean(xa % arma::square(z));

    const double f  = 1.0 / a + mu + (1.0 / alpha + 1.0) * beta;
    const double df = -1.0 / (a * a) + (gamma * alpha + beta * beta) / (alpha * alpha) + gamma;
    return f / df;
  };

  double a = a0;
  int iter = 0;
  for (iter = 1; iter <= maxit; ++iter) {
    double step = f_over_df(a);
    a -= step;
    if (a <= 0.0 || !std::isfinite(a)) {
      return {NA_REAL, NA_REAL, NA_REAL, iter};
    }
    if (std::abs(step) < (std::abs(a) * tol + tol)) break;
  }

  const double b = -static_cast<double>(n) / arma::accu(arma::log1p(-arma::pow(x, a)));
  const double logLik = static_cast<double>(n) * (std::log(a) + std::log(b)) +
    (a - 1.0) * arma::accu(arma::log(x)) + (b - 1.0) * arma::accu(arma::log1p(-arma::pow(x, a)));

  return {a, b, logLik, iter};
}

// ---------- Inverse Burr ----------
struct InvBurrResult {
  double shape1;
  double shape2;
  double logLik;
  int iterations;
};

inline InvBurrResult invburr_mle(const arma::vec& x,
                                 double tol = kTolDefault,
                                 int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) return {0.0, 0.0, NA_REAL, 0};

  arma::vec invx = 1.0 / x;
  double m1 = arma::mean(arma::log(invx));
  double m2 = arma::var(arma::log(invx), 1);

  BurrLmomResult init = burr_lmom_core(m1, m2, true);
  if (!std::isfinite(init.c) || init.c <= 0.0) {
    return {NA_REAL, NA_REAL, NA_REAL, 0};
  }
  BurrMLE res = burr_mle_core(invx, init.c, tol, maxit);

  const double c = res.shape1;
  const double k = res.shape2;
  const double logLik = static_cast<double>(n) * (std::log(c * k))
    - (c + 1.0) * arma::accu(arma::log(x))
    - (k + 1.0) * arma::accu(arma::log1p(arma::pow(x, -c)));

  return {c, k, logLik, res.iterations};
}

} // namespace mlcore
