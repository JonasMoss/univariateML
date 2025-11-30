#include <Rcpp.h>
#include <cmath>
#include <limits>

struct GumbelResult {
  double mu;
  double sigma;
  int iterations;
};

// Pure C++ core: Newton iterations on log(sigma), then compute mu.
static GumbelResult fit_gumbel_newton(const Rcpp::NumericVector& x) {
  const int n = x.size();
  const double* xp = x.begin();

  double sum_x = 0.0;
  double sum_x2 = 0.0;
  for (int i = 0; i < n; ++i) {
    double xi = xp[i];
    sum_x  += xi;
    sum_x2 += xi * xi;
  }
  const double xbar = sum_x / n;
  const double var0 = sum_x2 / n - xbar * xbar;

  double theta = std::log(std::sqrt(var0) * 2.45 / M_PI);
  const int maxit = 100;
  const double reltol = std::sqrt(std::numeric_limits<double>::epsilon());
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
    if (std::abs(dth) < std::abs(theta) * reltol) break;
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
Rcpp::NumericVector ml_gumbel_rcpp(const Rcpp::NumericVector& x) {
  GumbelResult res = fit_gumbel_newton(x);
  Rcpp::NumericVector out(3);
  out[0] = res.mu;
  out[1] = res.sigma;
  out[2] = res.iterations;
  out.names() = Rcpp::CharacterVector::create("mu", "sigma", "iterations");
  return out;
}
