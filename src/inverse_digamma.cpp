#include <RcppArmadillo.h>
#include <Rmath.h>
#include <cmath>
#include <limits>

namespace {

constexpr double GAMMA_C  = 0.57721566490153286060;
constexpr double ZETA3_C  = 1.2020569031595942854;
constexpr double PI_C     = M_PI;
constexpr double PI_SQ_C  = PI_C * PI_C;
constexpr double PSI1_C   = -0.57721566490153286060;     // digamma(1)
constexpr double PSI1_1_C = 1.6449340668482264365;      // trigamma(1)

double inverse_digamma_scalar(double x) {
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

double logmom1_c_core(double mu, double sigma2,
                      double tol,
                      int max_iter) {
  double c = PI_C / std::sqrt(3.0 * sigma2);
  for (int iter = 0; iter < max_iter; ++iter) {
    double k     = inverse_digamma_scalar(PSI1_C - c * mu);
    double psi1k = Rf_psigamma(k, 1);
    double psi2k = Rf_psigamma(k, 2);

    double c_inv = 1.0 / c;
    double c2    = c_inv * c_inv;
    double c3    = c2 * c_inv;
    double f     = c2 * (PSI1_1_C + psi1k) - sigma2;
    double dk_dc = -mu / psi1k;
    double df    = -2.0 * c3 * (PSI1_1_C + psi1k) + c2 * psi2k * dk_dc;

    double step = f / df;
    c -= step;
    if (std::fabs(step) < tol) {
      break;
    }
  }
  return c;
}

struct BurrMLE {
  double shape1;
  double shape2;
  int iterations;
};

BurrMLE burr_mle_from_c(const arma::vec& x,
                        double c_start,
                        double tol,
                        int max_iter) {
  const int    n     = static_cast<int>(x.n_elem);
  const double nd    = static_cast<double>(n);
  arma::vec log_x    = arma::log(x);
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
  double g_final     = arma::sum(arma::log1p(xc_final));
  double k           = nd / g_final;

  return {k, c, (iter == max_iter ? max_iter : iter)};
}

} // namespace

// [[Rcpp::export]]
double logmom1_c(double mu, double sigma2,
                 double tol = 1.220703e-4,
                 int max_iter = 100) {
  return logmom1_c_core(mu, sigma2, tol, max_iter);
}

// [[Rcpp::export]]
Rcpp::NumericVector mlburr_cpp(const arma::vec& x,
                               double tol = 1.220703e-4,
                               int max_iter = 100) {
  const double nd = static_cast<double>(x.n_elem);
  arma::vec log_x = arma::log(x);
  double m1 = arma::accu(log_x) / nd;
  double m2 = (arma::dot(log_x, log_x) / nd) - m1 * m1;

  double c_start = (m1 * m1 > m2 && m1 > 0)
    ? PI_C / std::sqrt(3 * m2)
    : logmom1_c_core(m1, m2, tol, max_iter);

  auto res = burr_mle_from_c(x, c_start, tol, max_iter);
  Rcpp::NumericVector out = Rcpp::NumericVector::create(res.shape1, res.shape2);
  out.names() = Rcpp::CharacterVector::create("shape1", "shape2");
  return out;
}
