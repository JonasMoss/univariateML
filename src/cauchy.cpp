// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>

struct CauchyResult {
  double mu;
  double scale;
  int iterations;
};

static CauchyResult fit_cauchy(const arma::vec& x, double tol = 1e-8, int maxit = 100) {
  int n = static_cast<int>(x.n_elem);
  if (n == 0) Rcpp::stop("x must have positive length");

  std::vector<double> data(x.begin(), x.end());
  std::nth_element(data.begin(), data.begin() + n/2, data.end());
  double mu = data[n/2];

  for (int i = 0; i < n; ++i) {
    data[i] = std::fabs(data[i] - mu);
  }
  std::nth_element(data.begin(), data.begin() + n/2, data.end());
  double theta = std::log(data[n/2] + std::numeric_limits<double>::epsilon());

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

// [[Rcpp::export]]
Rcpp::NumericVector mlcauchy_rcpp(const arma::vec& x,
                   double tol   = 1e-8,
                   int    maxit = 100) {
  CauchyResult res = fit_cauchy(x, tol, maxit);
  Rcpp::NumericVector out = Rcpp::NumericVector::create(
    Rcpp::_["mu"]    = res.mu,
    Rcpp::_["scale"] = res.scale,
    Rcpp::_["iterations"] = res.iterations
  );
  return out;
}
