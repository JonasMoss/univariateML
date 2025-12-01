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

} // namespace mlcore
