// [[Rcpp::depends(RcppArmadillo)]]
#include "ml_common.hpp"
#include <RcppArmadillo.h>

namespace {

constexpr double GAMMA_C = 0.57721566490153286060;
constexpr double ZETA3_C = 1.2020569031595942854;
constexpr double PI_C = M_PI;
constexpr double PI_SQ_C = PI_C * PI_C;
constexpr double PSI1_C = -0.57721566490153286060; // digamma(1)
constexpr double PSI1_1_C = 1.6449340668482264365; // trigamma(1)

double inverse_digamma_scalar(double x) {
  double y, r;
  if (x + GAMMA_C < -30.0) {
    double t = x + GAMMA_C;
    double t_inv = 1.0 / t;
    double t3 = t_inv * t_inv * t_inv;
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
    if (std::fabs(r) < 1e-14)
      break;
    y -= r / Rf_psigamma(y, 1);
  }
  return y;
}

double logmom1_c_core(double mu, double sigma2, double tol, int max_iter) {
  double c = PI_C / std::sqrt(3.0 * sigma2);
  for (int iter = 0; iter < max_iter; ++iter) {
    double k = inverse_digamma_scalar(PSI1_C - c * mu);
    double psi1k = Rf_psigamma(k, 1);
    double psi2k = Rf_psigamma(k, 2);

    double c_inv = 1.0 / c;
    double c2 = c_inv * c_inv;
    double c3 = c2 * c_inv;
    double f = c2 * (PSI1_1_C + psi1k) - sigma2;
    double dk_dc = -mu / psi1k;
    double df = -2.0 * c3 * (PSI1_1_C + psi1k) + c2 * psi2k * dk_dc;

    double step = f / df;
    c -= step;
    if (std::fabs(step) < tol) {
      break;
    }
  }
  return c;
}

} // namespace

// [[Rcpp::export]]
double logmom1_c(double mu, double sigma2, double tol = kTolDefault,
                 int maxit = kMaxIterDefault) {
  return logmom1_c_core(mu, sigma2, tol, maxit);
}
