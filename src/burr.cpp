// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <cmath>
#include <limits>
#include "ml_common.hpp"

namespace {

constexpr double NEULER_CST = -0.5772156649015328606; // digamma(1)
constexpr double BASEL_CST  = M_PI * M_PI / 6.0;      // trigamma(1)

inline double logit(double p) {
  return std::log(p / (1.0 - p));
}

inline double logistic(double x) {
  const double e = std::exp(-x);
  return 1.0 / (1.0 + e);
}

inline double logistic_pdf(double x) {
  const double L = logistic(x);
  return L * (1.0 - L);
}

struct RootResult {
  double k;
  int iterations;
};

RootResult invert_lower_logit(double cv) {
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

  double x = logit(k0);
  const double reltol = kTolDefault;
  const int    maxit  = kMaxIterDefault;

  int iter = 0;
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
    if (std::abs(x1 - x) < (std::abs(x) * reltol + 1e-3 * reltol)) {
      x = x1;
      break;
    }
    x = x1;
  }

  return {logistic(x), iter + 1};
}

RootResult invert_lower_log(double cv) {
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

  double p         = std::log(k0);
  const double rel = kTolDefault;
  const int    maxit = kMaxIterDefault;

  int iter = 0;
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
    if (std::abs(p1 - p) < (std::abs(p) * rel + 1e-3 * rel)) {
      p = p1;
      break;
    }
    p = p1;
  }

  return {std::exp(p), iter + 1};
}

RootResult invert_upper(double cv) {
  const double thr = 0.2;
  const double eps = 1e-9;

  double k0;
  if (cv < thr) {
    k0 = 1.0 + std::sqrt(2.0 * cv / BASEL_CST);
  } else {
    k0 = std::exp(std::sqrt(cv * BASEL_CST));
  }
  k0 = std::max(1.0 + eps, k0);

  double x = std::log(k0 - 1.0);
  const double reltol = kTolDefault;
  const int    maxit  = kMaxIterDefault;

  int iter = 0;
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
    if (std::abs(x1 - x) < (std::abs(x) * reltol + 1e-3 * reltol)) {
      x = x1;
      break;
    }
    x = x1;
  }

  return {std::exp(x) + 1.0, iter + 1};
}

struct BurrLmomResult {
  double k;
  double c;
  int iterations;
};

BurrLmomResult burr_lmom_core(double mu, double sigma2, bool use_logit_lower = true) {
  double k_hat = NA_REAL;
  double c_hat = NA_REAL;
  int iter = 0;

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

struct BurrMLE {
  double shape1;
  double shape2;
  int iterations;
};

BurrMLE burr_mle_core(const arma::vec& x,
                      double c_start,
                      double tol,
                      int max_iter) {
  const int    n   = static_cast<int>(x.n_elem);
  const double nd  = static_cast<double>(n);
  arma::vec    log_x = arma::log(x);
  const double* lxp  = log_x.memptr();
  const double  sum_l = arma::sum(log_x);

  double logc = std::log(c_start);
  int iter = 0;
  for (; iter < max_iter; ++iter) {
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
  double g_final = arma::sum(arma::log1p(xc_final));
  double k = nd / g_final;

  return {k, c, (iter == max_iter ? max_iter : iter)};
}

} // namespace

// [[Rcpp::export]]
Rcpp::NumericVector f_lower_inv_rcpp2(double cv) {
  auto res = invert_lower_logit(cv);
  return Rcpp::NumericVector::create(
    Rcpp::_["k"]    = res.k,
    Rcpp::_["iter"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector f_lower_inv_rcpp(double cv) {
  auto res = invert_lower_log(cv);
  return Rcpp::NumericVector::create(
    Rcpp::_["k"]    = res.k,
    Rcpp::_["iter"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector f_upper_inv_rcpp(double cv) {
  auto res = invert_upper(cv);
  return Rcpp::NumericVector::create(
    Rcpp::_["k"]    = res.k,
    Rcpp::_["iter"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector burr_lmom2(double mu, double sigma2) {
  auto res = burr_lmom_core(mu, sigma2, false);
  return Rcpp::NumericVector::create(
    Rcpp::_["k"]    = res.k,
    Rcpp::_["c"]    = res.c,
    Rcpp::_["iter"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector burr_lmom(double mu, double sigma2) {
  auto res = burr_lmom_core(mu, sigma2, true);
  return Rcpp::NumericVector::create(
    Rcpp::_["k"]    = res.k,
    Rcpp::_["c"]    = res.c,
    Rcpp::_["iter"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::NumericVector mlburr_rcpp2(const arma::vec& x,
                                 double tol = 1.220703e-4,
                                 int max_iter = 100) {
  const double nd    = static_cast<double>(x.n_elem);
  arma::vec log_x    = arma::log(x);
  double m1          = arma::accu(log_x) / nd;
  double m2          = (arma::dot(log_x, log_x) / nd) - m1 * m1;

  double c0 = burr_lmom_core(m1, m2, true).c;
  if (Rcpp::NumericVector::is_na(c0) || c0 <= 0.0) {
    Rcpp::stop("Failed to compute starting value for c");
  }

  auto res = burr_mle_core(x, c0, tol, max_iter);
  Rcpp::NumericVector out = Rcpp::NumericVector::create(res.shape1, res.shape2);
  out.names() = Rcpp::CharacterVector::create("shape1", "shape2");
  return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector mlburr_rcpp(const arma::vec& x,
                                Rcpp::Nullable<Rcpp::NumericVector> init = R_NilValue,
                                double tol = 1.220703e-4,
                                int max_iter = 100) {
  const double nd    = static_cast<double>(x.n_elem);
  arma::vec log_x    = arma::log(x);
  double m1          = arma::accu(log_x) / nd;
  double m2          = (arma::dot(log_x, log_x) / nd) - m1 * m1;

  double c0;
  if (init.isNotNull()) {
    Rcpp::NumericVector iv(init);
    if (iv.size() < 1 || Rcpp::NumericVector::is_na(iv[0]) || iv[0] <= 0) {
      Rcpp::stop("`init` must be a length-1 numeric vector with a positive c");
    }
    c0 = iv[0];
  } else {
    c0 = burr_lmom_core(m1, m2, true).c;
    if (Rcpp::NumericVector::is_na(c0) || c0 <= 0.0) {
      Rcpp::stop("Failed to get a valid initial c from burr_lmom()");
    }
  }

  auto res = burr_mle_core(x, c0, tol, max_iter);
  return Rcpp::NumericVector::create(
    Rcpp::_["shape1"] = res.shape1,
    Rcpp::_["shape2"] = res.shape2,
    Rcpp::_["iter"]   = res.iterations
  );
}
