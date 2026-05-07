// Aggregator for distribution exports; pulls in one file per density.
// [[Rcpp::depends(RcppArmadillo)]]

#include "distributions/common.h"

// Pull in the actual definitions.
#include "distributions/nakagami.cpp"
#include "distributions/logitnorm.cpp"
#include "distributions/zipf.cpp"
#include "distributions/ged.cpp"
#include "distributions/sged.cpp"
#include "distributions/snorm.cpp"
#include "distributions/std.cpp"
#include "distributions/sstd.cpp"
// Phase C tier 1: closed-form ports of extraDistr/actuar distributions.
#include "distributions/laplace.cpp"
#include "distributions/rayleigh.cpp"
#include "distributions/dunif.cpp"
#include "distributions/power.cpp"
#include "distributions/pareto.cpp"
#include "distributions/lomax.cpp"
#include "distributions/gumbel.cpp"
#include "distributions/kumar.cpp"
// Phase C tier 2: Boost-backed transforms.
#include "distributions/invgamma.cpp"
#include "distributions/betapr.cpp"
#include "distributions/invgauss.cpp"
#include "distributions/llogis.cpp"
#include "distributions/invweibull.cpp"

// Exported wrappers forwarding to dist_impl.
// [[Rcpp::export]]
arma::vec dnaka(const arma::vec &x, const arma::vec &shape, const arma::vec &scale, bool log = false) {
  return dist_impl::dnaka(x, shape, scale, log);
}
// [[Rcpp::export]]
arma::vec pnaka(const arma::vec &q, const arma::vec &shape, const arma::vec &scale,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::pnaka(q, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qnaka(const arma::vec &p, const arma::vec &shape, const arma::vec &scale,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::qnaka(p, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rnaka(int n, double shape, double scale) {
  return dist_impl::rnaka(n, shape, scale);
}

// [[Rcpp::export]]
arma::vec dlogitnorm(const arma::vec &x, const arma::vec &mu, const arma::vec &sigma, bool log = false) {
  return dist_impl::dlogitnorm(x, mu, sigma, log);
}
// [[Rcpp::export]]
arma::vec plogitnorm(const arma::vec &q, const arma::vec &mu, const arma::vec &sigma,
                     bool lower_tail = true, bool log_p = false) {
  return dist_impl::plogitnorm(q, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qlogitnorm(const arma::vec &p, const arma::vec &mu, const arma::vec &sigma,
                     bool lower_tail = true, bool log_p = false) {
  return dist_impl::qlogitnorm(p, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rlogitnorm(int n, double mu = 0.0, double sigma = 1.0) {
  return dist_impl::rlogitnorm(n, mu, sigma);
}

// [[Rcpp::export]]
arma::vec dzipf(const arma::vec &x, const arma::vec &N, const arma::vec &s, bool log = false) {
  return dist_impl::dzipf(x, N, s, log);
}
// [[Rcpp::export]]
arma::vec pzipf(const arma::vec &q, const arma::vec &N, const arma::vec &s,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::pzipf(q, N, s, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qzipf(const arma::vec &p, const arma::vec &N, const arma::vec &s,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::qzipf(p, N, s, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rzipf(int n, double N, double s) { return dist_impl::rzipf(n, N, s); }

// [[Rcpp::export]]
arma::vec dged(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool log = false) {
  return dist_impl::dged(x, mean, sd, nu, log);
}
// [[Rcpp::export]]
arma::vec pged(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::pged(q, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qged(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::qged(p, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rged(int n, double mean = 0.0, double sd = 1.0, double nu = 2.0) {
  return dist_impl::rged(n, mean, sd, nu);
}

// [[Rcpp::export]]
arma::vec dsged(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi, bool log = false) {
  return dist_impl::dsged(x, mean, sd, nu, xi, log);
}
// [[Rcpp::export]]
arma::vec psged(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi) {
  return dist_impl::psged(q, mean, sd, nu, xi);
}
// [[Rcpp::export]]
arma::vec qsged(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi) {
  return dist_impl::qsged(p, mean, sd, nu, xi);
}
// [[Rcpp::export]]
arma::vec rsged(int n, double mean = 0.0, double sd = 1.0, double nu = 2.0, double xi = 1.5) {
  return dist_impl::rsged(n, mean, sd, nu, xi);
}

// [[Rcpp::export]]
arma::vec dsnorm(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &xi,
                 bool log = false) {
  return dist_impl::dsnorm(x, mean, sd, xi, log);
}
// [[Rcpp::export]]
arma::vec psnorm(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &xi) {
  return dist_impl::psnorm(q, mean, sd, xi);
}
// [[Rcpp::export]]
arma::vec qsnorm(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &xi) {
  return dist_impl::qsnorm(p, mean, sd, xi);
}
// [[Rcpp::export]]
arma::vec rsnorm(int n, double mean = 0.0, double sd = 1.0, double xi = 1.5) {
  return dist_impl::rsnorm(n, mean, sd, xi);
}

// [[Rcpp::export]]
arma::vec dstd(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool log = false) {
  return dist_impl::dstd(x, mean, sd, nu, log);
}
// [[Rcpp::export]]
arma::vec pstd(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::pstd(q, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qstd(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::qstd(p, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0) {
  return dist_impl::rstd(n, mean, sd, nu);
}

// [[Rcpp::export]]
arma::vec dsstd(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi, bool log = false) {
  return dist_impl::dsstd(x, mean, sd, nu, xi, log);
}
// [[Rcpp::export]]
arma::vec psstd(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi) {
  return dist_impl::psstd(q, mean, sd, nu, xi);
}
// [[Rcpp::export]]
arma::vec qsstd(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi) {
  return dist_impl::qsstd(p, mean, sd, nu, xi);
}
// [[Rcpp::export]]
arma::vec rsstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0, double xi = 1.5) {
  return dist_impl::rsstd(n, mean, sd, nu, xi);
}

// --- Phase C tier 1: extraDistr ports ---

// [[Rcpp::export]]
arma::vec dlaplace(const arma::vec &x, const arma::vec &mu,
                   const arma::vec &sigma, bool log = false) {
  return dist_impl::dlaplace(x, mu, sigma, log);
}
// [[Rcpp::export]]
arma::vec plaplace(const arma::vec &q, const arma::vec &mu,
                   const arma::vec &sigma, bool lower_tail = true,
                   bool log_p = false) {
  return dist_impl::plaplace(q, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qlaplace(const arma::vec &p, const arma::vec &mu,
                   const arma::vec &sigma, bool lower_tail = true,
                   bool log_p = false) {
  return dist_impl::qlaplace(p, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rlaplace(int n, double mu = 0.0, double sigma = 1.0) {
  return dist_impl::rlaplace(n, mu, sigma);
}

// [[Rcpp::export]]
arma::vec drayleigh(const arma::vec &x, const arma::vec &sigma,
                    bool log = false) {
  return dist_impl::drayleigh(x, sigma, log);
}
// [[Rcpp::export]]
arma::vec prayleigh(const arma::vec &q, const arma::vec &sigma,
                    bool lower_tail = true, bool log_p = false) {
  return dist_impl::prayleigh(q, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qrayleigh(const arma::vec &p, const arma::vec &sigma,
                    bool lower_tail = true, bool log_p = false) {
  return dist_impl::qrayleigh(p, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rrayleigh(int n, double sigma = 1.0) {
  return dist_impl::rrayleigh(n, sigma);
}

// [[Rcpp::export]]
arma::vec ddunif(const arma::vec &x, const arma::vec &min,
                 const arma::vec &max, bool log = false) {
  return dist_impl::ddunif(x, min, max, log);
}
// [[Rcpp::export]]
arma::vec pdunif(const arma::vec &q, const arma::vec &min,
                 const arma::vec &max, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::pdunif(q, min, max, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qdunif(const arma::vec &p, const arma::vec &min,
                 const arma::vec &max, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::qdunif(p, min, max, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rdunif(int n, double min, double max) {
  return dist_impl::rdunif(n, min, max);
}

// [[Rcpp::export]]
arma::vec dpower(const arma::vec &x, const arma::vec &alpha,
                 const arma::vec &beta, bool log = false) {
  return dist_impl::dpower(x, alpha, beta, log);
}
// [[Rcpp::export]]
arma::vec ppower(const arma::vec &q, const arma::vec &alpha,
                 const arma::vec &beta, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::ppower(q, alpha, beta, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qpower(const arma::vec &p, const arma::vec &alpha,
                 const arma::vec &beta, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::qpower(p, alpha, beta, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rpower(int n, double alpha = 1.0, double beta = 1.0) {
  return dist_impl::rpower(n, alpha, beta);
}

// [[Rcpp::export]]
arma::vec dpareto(const arma::vec &x, const arma::vec &a,
                  const arma::vec &b, bool log = false) {
  return dist_impl::dpareto(x, a, b, log);
}
// [[Rcpp::export]]
arma::vec ppareto(const arma::vec &q, const arma::vec &a,
                  const arma::vec &b, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::ppareto(q, a, b, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qpareto(const arma::vec &p, const arma::vec &a,
                  const arma::vec &b, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::qpareto(p, a, b, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rpareto(int n, double a = 1.0, double b = 1.0) {
  return dist_impl::rpareto(n, a, b);
}

// [[Rcpp::export]]
arma::vec dlomax(const arma::vec &x, const arma::vec &lambda,
                 const arma::vec &kappa, bool log = false) {
  return dist_impl::dlomax(x, lambda, kappa, log);
}
// [[Rcpp::export]]
arma::vec plomax(const arma::vec &q, const arma::vec &lambda,
                 const arma::vec &kappa, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::plomax(q, lambda, kappa, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qlomax(const arma::vec &p, const arma::vec &lambda,
                 const arma::vec &kappa, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::qlomax(p, lambda, kappa, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rlomax(int n, double lambda = 1.0, double kappa = 1.0) {
  return dist_impl::rlomax(n, lambda, kappa);
}

// [[Rcpp::export]]
arma::vec dgumbel(const arma::vec &x, const arma::vec &mu,
                  const arma::vec &sigma, bool log = false) {
  return dist_impl::dgumbel(x, mu, sigma, log);
}
// [[Rcpp::export]]
arma::vec pgumbel(const arma::vec &q, const arma::vec &mu,
                  const arma::vec &sigma, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::pgumbel(q, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qgumbel(const arma::vec &p, const arma::vec &mu,
                  const arma::vec &sigma, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::qgumbel(p, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rgumbel(int n, double mu = 0.0, double sigma = 1.0) {
  return dist_impl::rgumbel(n, mu, sigma);
}

// [[Rcpp::export]]
arma::vec dkumar(const arma::vec &x, const arma::vec &a, const arma::vec &b,
                 bool log = false) {
  return dist_impl::dkumar(x, a, b, log);
}
// [[Rcpp::export]]
arma::vec pkumar(const arma::vec &q, const arma::vec &a, const arma::vec &b,
                 bool lower_tail = true, bool log_p = false) {
  return dist_impl::pkumar(q, a, b, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qkumar(const arma::vec &p, const arma::vec &a, const arma::vec &b,
                 bool lower_tail = true, bool log_p = false) {
  return dist_impl::qkumar(p, a, b, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rkumar(int n, double a = 1.0, double b = 1.0) {
  return dist_impl::rkumar(n, a, b);
}

// --- Phase C tier 2: Boost-backed transforms ---

// [[Rcpp::export]]
arma::vec dinvgamma(const arma::vec &x, const arma::vec &alpha,
                    const arma::vec &beta, bool log = false) {
  return dist_impl::dinvgamma(x, alpha, beta, log);
}
// [[Rcpp::export]]
arma::vec pinvgamma(const arma::vec &q, const arma::vec &alpha,
                    const arma::vec &beta, bool lower_tail = true,
                    bool log_p = false) {
  return dist_impl::pinvgamma(q, alpha, beta, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qinvgamma(const arma::vec &p, const arma::vec &alpha,
                    const arma::vec &beta, bool lower_tail = true,
                    bool log_p = false) {
  return dist_impl::qinvgamma(p, alpha, beta, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rinvgamma(int n, double alpha = 1.0, double beta = 1.0) {
  return dist_impl::rinvgamma(n, alpha, beta);
}

// [[Rcpp::export]]
arma::vec dbetapr(const arma::vec &x, const arma::vec &shape1,
                  const arma::vec &shape2, bool log = false) {
  return dist_impl::dbetapr(x, shape1, shape2, log);
}
// [[Rcpp::export]]
arma::vec pbetapr(const arma::vec &q, const arma::vec &shape1,
                  const arma::vec &shape2, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::pbetapr(q, shape1, shape2, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qbetapr(const arma::vec &p, const arma::vec &shape1,
                  const arma::vec &shape2, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::qbetapr(p, shape1, shape2, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rbetapr(int n, double shape1 = 1.0, double shape2 = 1.0) {
  return dist_impl::rbetapr(n, shape1, shape2);
}

// [[Rcpp::export]]
arma::vec dinvgauss(const arma::vec &x, const arma::vec &mu,
                    const arma::vec &phi, bool log = false) {
  return dist_impl::dinvgauss(x, mu, phi, log);
}
// [[Rcpp::export]]
arma::vec pinvgauss(const arma::vec &q, const arma::vec &mu,
                    const arma::vec &phi, bool lower_tail = true,
                    bool log_p = false) {
  return dist_impl::pinvgauss(q, mu, phi, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qinvgauss(const arma::vec &p, const arma::vec &mu,
                    const arma::vec &phi, bool lower_tail = true,
                    bool log_p = false) {
  return dist_impl::qinvgauss(p, mu, phi, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rinvgauss(int n, double mu = 1.0, double phi = 1.0) {
  return dist_impl::rinvgauss(n, mu, phi);
}

// [[Rcpp::export]]
arma::vec dllogis(const arma::vec &x, const arma::vec &shape,
                  const arma::vec &scale, bool log = false) {
  return dist_impl::dllogis(x, shape, scale, log);
}
// [[Rcpp::export]]
arma::vec pllogis(const arma::vec &q, const arma::vec &shape,
                  const arma::vec &scale, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::pllogis(q, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qllogis(const arma::vec &p, const arma::vec &shape,
                  const arma::vec &scale, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::qllogis(p, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rllogis(int n, double shape, double scale) {
  return dist_impl::rllogis(n, shape, scale);
}

// [[Rcpp::export]]
arma::vec dinvweibull(const arma::vec &x, const arma::vec &shape,
                      const arma::vec &scale, bool log = false) {
  return dist_impl::dinvweibull(x, shape, scale, log);
}
// [[Rcpp::export]]
arma::vec pinvweibull(const arma::vec &q, const arma::vec &shape,
                      const arma::vec &scale, bool lower_tail = true,
                      bool log_p = false) {
  return dist_impl::pinvweibull(q, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qinvweibull(const arma::vec &p, const arma::vec &shape,
                      const arma::vec &scale, bool lower_tail = true,
                      bool log_p = false) {
  return dist_impl::qinvweibull(p, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rinvweibull(int n, double shape, double scale) {
  return dist_impl::rinvweibull(n, shape, scale);
}
