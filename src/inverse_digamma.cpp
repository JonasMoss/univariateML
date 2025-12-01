// [[Rcpp::depends(RcppArmadillo)]]
#include "ml_common.hpp"
#include <RcppArmadillo.h>

namespace {

constexpr double GAMMA_C   = 0.57721566490153286060;
constexpr double ZETA3_C   = 1.2020569031595942854;
constexpr double PI_C      = M_PI;
constexpr double PI_SQ_C   = PI_C * PI_C;
constexpr double PSI1_C    = -0.57721566490153286060; // digamma(1)
constexpr double PSI1_1_C  = 1.6449340668482264365;  // trigamma(1)

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

} // namespace

// [[Rcpp::export]]
Rcpp::NumericVector inverse_digamma(Rcpp::NumericVector x) {
  Rcpp::NumericVector out(x.size());
  for (int i = 0; i < x.size(); ++i) {
    out[i] = inverse_digamma_scalar(x[i]);
  }
  return out;
}
