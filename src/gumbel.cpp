// [[Rcpp::depends(RcppArmadillo)]]
#include "ml_common.hpp"
#include <RcppArmadillo.h>

struct GumbelResult {
  double mu;
  double sigma;
  int iterations;
};

using GumbelOutcome = std::variant<GumbelResult, std::string>;

// Pure C++ core: Newton iterations on log(sigma), then compute mu.
static GumbelOutcome fit_gumbel_newton(const arma::vec &x,
                                       double tol = kTolDefault,
                                       int maxit = kMaxIterDefault) {
  const int n = static_cast<int>(x.n_elem);
  const double *xp = x.memptr();

  if (n == 0) {
    return std::string("x must have positive length");
  }

  double sum_x = 0.0;
  double sum_x2 = 0.0;
  for (double xi : x) {
    sum_x += xi;
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
    psi2 = psi2 / psi0 - psi1 * psi1;

    const double f = sigma + psi1;
    const double df = 1.0 + psi2 * (invs * invs);
    const double dth = f / (df * sigma);

    theta -= dth;
    if (std::abs(dth) < std::abs(theta) * tol)
      break;
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

// [[Rcpp::export]]
Rcpp::NumericVector ml_gumbel_rcpp(const arma::vec &x, double tol = 1.220703e-4,
                                   int maxit = 100) {
  GumbelOutcome res = fit_gumbel_newton(x, tol, maxit);
  if (std::holds_alternative<std::string>(res)) {
    Rcpp::stop(std::get<std::string>(res));
  }
  GumbelResult ok = std::get<GumbelResult>(res);
  Rcpp::NumericVector out(3);
  out[0] = ok.mu;
  out[1] = ok.sigma;
  out[2] = ok.iterations;
  out.names() = Rcpp::CharacterVector::create("mu", "sigma", "iterations");
  return out;
}
