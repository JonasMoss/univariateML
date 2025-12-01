// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <cmath>
#include <stdexcept>
#include <string>
#include <variant>

namespace {

struct LogisticResult {
  double mu;
  double scale;
  int iterations;
};

using LogisticOutcome = std::variant<LogisticResult, std::string>;

LogisticOutcome logistic_mle(const arma::vec& x,
                             double tol,
                             int maxit) {
  const int n = static_cast<int>(x.n_elem);
  if (n == 0) {
    return std::string("x must have positive length");
  }

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
    if (std::abs(det) < 1e-14) {
      break;
    }

    double dmu    = ( J22 * U1 - J12 * U2) / det;
    double dtheta = (-J12 * U1 + J11 * U2) / det;

    mu    += dmu;
    theta += dtheta;

    if (std::max(std::fabs(dmu), std::fabs(dtheta)) < tol) {
      return LogisticResult{mu, std::exp(theta), iter + 1};
    }
  }

  return LogisticResult{mu, std::exp(theta), maxit};
}

} // namespace

// [[Rcpp::export]]
Rcpp::List mllogis_fast(const arma::vec& x,
                        double tol   = 1.220703e-4,
                        int    maxit = 100) {
  LogisticOutcome res = logistic_mle(x, tol, maxit);
  if (std::holds_alternative<std::string>(res)) {
    Rcpp::stop(std::get<std::string>(res));
  }
  LogisticResult ok = std::get<LogisticResult>(res);

  return Rcpp::List::create(
    Rcpp::_["estimates"]  = Rcpp::NumericVector::create(
      Rcpp::_["mu"]    = ok.mu,
      Rcpp::_["scale"] = ok.scale
    ),
    Rcpp::_["iterations"] = ok.iterations
  );
}
