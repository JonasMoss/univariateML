// Aggregator for distribution exports; pulls in one file per density.

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
// Phase C tier 3: actuarial closed-form.
#include "distributions/fatigue.cpp"
#include "distributions/gompertz.cpp"
#include "distributions/lgamma_dist.cpp"
#include "distributions/burr.cpp"
#include "distributions/invburr.cpp"
#include "distributions/paralogis.cpp"
// Phase C tier 4: special samplers.
#include "distributions/lgser.cpp"
#include "distributions/zip.cpp"

// Exported wrappers forwarding to dist_impl.
// [[Rcpp::export]]
Rcpp::NumericVector dnaka(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape, const Rcpp::NumericVector &scale, bool log = false) {
  return dist_impl::dnaka(x, shape, scale, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pnaka(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape, const Rcpp::NumericVector &scale,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::pnaka(q, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qnaka(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape, const Rcpp::NumericVector &scale,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::qnaka(p, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rnaka(int n, double shape, double scale) {
  return dist_impl::rnaka(n, shape, scale);
}

// [[Rcpp::export]]
Rcpp::NumericVector dlogitnorm(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mu, const Rcpp::NumericVector &sigma, bool log = false) {
  return dist_impl::dlogitnorm(x, mu, sigma, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector plogitnorm(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mu, const Rcpp::NumericVector &sigma,
                     bool lower_tail = true, bool log_p = false) {
  return dist_impl::plogitnorm(q, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qlogitnorm(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mu, const Rcpp::NumericVector &sigma,
                     bool lower_tail = true, bool log_p = false) {
  return dist_impl::qlogitnorm(p, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rlogitnorm(int n, double mu = 0.0, double sigma = 1.0) {
  return dist_impl::rlogitnorm(n, mu, sigma);
}

// [[Rcpp::export]]
Rcpp::NumericVector dzipf(const Rcpp::NumericVector &x, const Rcpp::NumericVector &N, const Rcpp::NumericVector &s, bool log = false) {
  return dist_impl::dzipf(x, N, s, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pzipf(const Rcpp::NumericVector &q, const Rcpp::NumericVector &N, const Rcpp::NumericVector &s,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::pzipf(q, N, s, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qzipf(const Rcpp::NumericVector &p, const Rcpp::NumericVector &N, const Rcpp::NumericVector &s,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::qzipf(p, N, s, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rzipf(int n, double N, double s) { return dist_impl::rzipf(n, N, s); }

// [[Rcpp::export]]
Rcpp::NumericVector dged(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
               bool log = false) {
  return dist_impl::dged(x, mean, sd, nu, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pged(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::pged(q, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qged(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::qged(p, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rged(int n, double mean = 0.0, double sd = 1.0, double nu = 2.0) {
  return dist_impl::rged(n, mean, sd, nu);
}

// [[Rcpp::export]]
Rcpp::NumericVector dsged(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
                const Rcpp::NumericVector &xi, bool log = false) {
  return dist_impl::dsged(x, mean, sd, nu, xi, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector psged(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
                const Rcpp::NumericVector &xi) {
  return dist_impl::psged(q, mean, sd, nu, xi);
}
// [[Rcpp::export]]
Rcpp::NumericVector qsged(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
                const Rcpp::NumericVector &xi) {
  return dist_impl::qsged(p, mean, sd, nu, xi);
}
// [[Rcpp::export]]
Rcpp::NumericVector rsged(int n, double mean = 0.0, double sd = 1.0, double nu = 2.0, double xi = 1.5) {
  return dist_impl::rsged(n, mean, sd, nu, xi);
}

// [[Rcpp::export]]
Rcpp::NumericVector dsnorm(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &xi,
                 bool log = false) {
  return dist_impl::dsnorm(x, mean, sd, xi, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector psnorm(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &xi) {
  return dist_impl::psnorm(q, mean, sd, xi);
}
// [[Rcpp::export]]
Rcpp::NumericVector qsnorm(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &xi) {
  return dist_impl::qsnorm(p, mean, sd, xi);
}
// [[Rcpp::export]]
Rcpp::NumericVector rsnorm(int n, double mean = 0.0, double sd = 1.0, double xi = 1.5) {
  return dist_impl::rsnorm(n, mean, sd, xi);
}

// [[Rcpp::export]]
Rcpp::NumericVector dstd(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
               bool log = false) {
  return dist_impl::dstd(x, mean, sd, nu, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pstd(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::pstd(q, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qstd(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::qstd(p, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0) {
  return dist_impl::rstd(n, mean, sd, nu);
}

// [[Rcpp::export]]
Rcpp::NumericVector dsstd(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
                const Rcpp::NumericVector &xi, bool log = false) {
  return dist_impl::dsstd(x, mean, sd, nu, xi, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector psstd(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
                const Rcpp::NumericVector &xi) {
  return dist_impl::psstd(q, mean, sd, nu, xi);
}
// [[Rcpp::export]]
Rcpp::NumericVector qsstd(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mean, const Rcpp::NumericVector &sd, const Rcpp::NumericVector &nu,
                const Rcpp::NumericVector &xi) {
  return dist_impl::qsstd(p, mean, sd, nu, xi);
}
// [[Rcpp::export]]
Rcpp::NumericVector rsstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0, double xi = 1.5) {
  return dist_impl::rsstd(n, mean, sd, nu, xi);
}

// --- Phase C tier 1: extraDistr ports ---

// [[Rcpp::export]]
Rcpp::NumericVector dlaplace(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mu,
                   const Rcpp::NumericVector &sigma, bool log = false) {
  return dist_impl::dlaplace(x, mu, sigma, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector plaplace(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mu,
                   const Rcpp::NumericVector &sigma, bool lower_tail = true,
                   bool log_p = false) {
  return dist_impl::plaplace(q, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qlaplace(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mu,
                   const Rcpp::NumericVector &sigma, bool lower_tail = true,
                   bool log_p = false) {
  return dist_impl::qlaplace(p, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rlaplace(int n, double mu = 0.0, double sigma = 1.0) {
  return dist_impl::rlaplace(n, mu, sigma);
}

// [[Rcpp::export]]
Rcpp::NumericVector drayleigh(const Rcpp::NumericVector &x, const Rcpp::NumericVector &sigma,
                    bool log = false) {
  return dist_impl::drayleigh(x, sigma, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector prayleigh(const Rcpp::NumericVector &q, const Rcpp::NumericVector &sigma,
                    bool lower_tail = true, bool log_p = false) {
  return dist_impl::prayleigh(q, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qrayleigh(const Rcpp::NumericVector &p, const Rcpp::NumericVector &sigma,
                    bool lower_tail = true, bool log_p = false) {
  return dist_impl::qrayleigh(p, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rrayleigh(int n, double sigma = 1.0) {
  return dist_impl::rrayleigh(n, sigma);
}

// [[Rcpp::export]]
Rcpp::NumericVector ddunif(const Rcpp::NumericVector &x, const Rcpp::NumericVector &min,
                 const Rcpp::NumericVector &max, bool log = false) {
  return dist_impl::ddunif(x, min, max, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pdunif(const Rcpp::NumericVector &q, const Rcpp::NumericVector &min,
                 const Rcpp::NumericVector &max, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::pdunif(q, min, max, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qdunif(const Rcpp::NumericVector &p, const Rcpp::NumericVector &min,
                 const Rcpp::NumericVector &max, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::qdunif(p, min, max, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rdunif(int n, double min, double max) {
  return dist_impl::rdunif(n, min, max);
}

// [[Rcpp::export]]
Rcpp::NumericVector dpower(const Rcpp::NumericVector &x, const Rcpp::NumericVector &alpha,
                 const Rcpp::NumericVector &beta, bool log = false) {
  return dist_impl::dpower(x, alpha, beta, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector ppower(const Rcpp::NumericVector &q, const Rcpp::NumericVector &alpha,
                 const Rcpp::NumericVector &beta, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::ppower(q, alpha, beta, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qpower(const Rcpp::NumericVector &p, const Rcpp::NumericVector &alpha,
                 const Rcpp::NumericVector &beta, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::qpower(p, alpha, beta, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rpower(int n, double alpha = 1.0, double beta = 1.0) {
  return dist_impl::rpower(n, alpha, beta);
}

// [[Rcpp::export]]
Rcpp::NumericVector dpareto(const Rcpp::NumericVector &x, const Rcpp::NumericVector &a,
                  const Rcpp::NumericVector &b, bool log = false) {
  return dist_impl::dpareto(x, a, b, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector ppareto(const Rcpp::NumericVector &q, const Rcpp::NumericVector &a,
                  const Rcpp::NumericVector &b, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::ppareto(q, a, b, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qpareto(const Rcpp::NumericVector &p, const Rcpp::NumericVector &a,
                  const Rcpp::NumericVector &b, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::qpareto(p, a, b, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rpareto(int n, double a = 1.0, double b = 1.0) {
  return dist_impl::rpareto(n, a, b);
}

// [[Rcpp::export]]
Rcpp::NumericVector dlomax(const Rcpp::NumericVector &x, const Rcpp::NumericVector &lambda,
                 const Rcpp::NumericVector &kappa, bool log = false) {
  return dist_impl::dlomax(x, lambda, kappa, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector plomax(const Rcpp::NumericVector &q, const Rcpp::NumericVector &lambda,
                 const Rcpp::NumericVector &kappa, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::plomax(q, lambda, kappa, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qlomax(const Rcpp::NumericVector &p, const Rcpp::NumericVector &lambda,
                 const Rcpp::NumericVector &kappa, bool lower_tail = true,
                 bool log_p = false) {
  return dist_impl::qlomax(p, lambda, kappa, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rlomax(int n, double lambda = 1.0, double kappa = 1.0) {
  return dist_impl::rlomax(n, lambda, kappa);
}

// [[Rcpp::export]]
Rcpp::NumericVector dgumbel(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mu,
                  const Rcpp::NumericVector &sigma, bool log = false) {
  return dist_impl::dgumbel(x, mu, sigma, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pgumbel(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mu,
                  const Rcpp::NumericVector &sigma, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::pgumbel(q, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qgumbel(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mu,
                  const Rcpp::NumericVector &sigma, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::qgumbel(p, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rgumbel(int n, double mu = 0.0, double sigma = 1.0) {
  return dist_impl::rgumbel(n, mu, sigma);
}

// [[Rcpp::export]]
Rcpp::NumericVector dkumar(const Rcpp::NumericVector &x, const Rcpp::NumericVector &a, const Rcpp::NumericVector &b,
                 bool log = false) {
  return dist_impl::dkumar(x, a, b, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pkumar(const Rcpp::NumericVector &q, const Rcpp::NumericVector &a, const Rcpp::NumericVector &b,
                 bool lower_tail = true, bool log_p = false) {
  return dist_impl::pkumar(q, a, b, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qkumar(const Rcpp::NumericVector &p, const Rcpp::NumericVector &a, const Rcpp::NumericVector &b,
                 bool lower_tail = true, bool log_p = false) {
  return dist_impl::qkumar(p, a, b, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rkumar(int n, double a = 1.0, double b = 1.0) {
  return dist_impl::rkumar(n, a, b);
}

// --- Phase C tier 2: Boost-backed transforms ---

// [[Rcpp::export]]
Rcpp::NumericVector dinvgamma(const Rcpp::NumericVector &x, const Rcpp::NumericVector &alpha,
                    const Rcpp::NumericVector &beta, bool log = false) {
  return dist_impl::dinvgamma(x, alpha, beta, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pinvgamma(const Rcpp::NumericVector &q, const Rcpp::NumericVector &alpha,
                    const Rcpp::NumericVector &beta, bool lower_tail = true,
                    bool log_p = false) {
  return dist_impl::pinvgamma(q, alpha, beta, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qinvgamma(const Rcpp::NumericVector &p, const Rcpp::NumericVector &alpha,
                    const Rcpp::NumericVector &beta, bool lower_tail = true,
                    bool log_p = false) {
  return dist_impl::qinvgamma(p, alpha, beta, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rinvgamma(int n, double alpha = 1.0, double beta = 1.0) {
  return dist_impl::rinvgamma(n, alpha, beta);
}

// [[Rcpp::export]]
Rcpp::NumericVector dbetapr(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape1,
                  const Rcpp::NumericVector &shape2, bool log = false) {
  return dist_impl::dbetapr(x, shape1, shape2, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pbetapr(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape1,
                  const Rcpp::NumericVector &shape2, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::pbetapr(q, shape1, shape2, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qbetapr(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape1,
                  const Rcpp::NumericVector &shape2, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::qbetapr(p, shape1, shape2, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rbetapr(int n, double shape1 = 1.0, double shape2 = 1.0) {
  return dist_impl::rbetapr(n, shape1, shape2);
}

// [[Rcpp::export]]
Rcpp::NumericVector dinvgauss(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mean,
                    const Rcpp::NumericVector &shape, bool log = false) {
  return dist_impl::dinvgauss(x, mean, shape, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pinvgauss(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mean,
                    const Rcpp::NumericVector &shape, bool lower_tail = true,
                    bool log_p = false) {
  return dist_impl::pinvgauss(q, mean, shape, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qinvgauss(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mean,
                    const Rcpp::NumericVector &shape, bool lower_tail = true,
                    bool log_p = false) {
  return dist_impl::qinvgauss(p, mean, shape, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rinvgauss(int n, double mean = 1.0, double shape = 1.0) {
  return dist_impl::rinvgauss(n, mean, shape);
}

// [[Rcpp::export]]
Rcpp::NumericVector dllogis(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape,
                  const Rcpp::NumericVector &rate, bool log = false) {
  return dist_impl::dllogis(x, shape, rate, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pllogis(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape,
                  const Rcpp::NumericVector &rate, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::pllogis(q, shape, rate, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qllogis(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape,
                  const Rcpp::NumericVector &rate, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::qllogis(p, shape, rate, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rllogis(int n, double shape, double rate) {
  return dist_impl::rllogis(n, shape, rate);
}

// [[Rcpp::export]]
Rcpp::NumericVector dinvweibull(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape,
                      const Rcpp::NumericVector &rate, bool log = false) {
  return dist_impl::dinvweibull(x, shape, rate, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pinvweibull(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape,
                      const Rcpp::NumericVector &rate, bool lower_tail = true,
                      bool log_p = false) {
  return dist_impl::pinvweibull(q, shape, rate, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qinvweibull(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape,
                      const Rcpp::NumericVector &rate, bool lower_tail = true,
                      bool log_p = false) {
  return dist_impl::qinvweibull(p, shape, rate, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rinvweibull(int n, double shape, double rate) {
  return dist_impl::rinvweibull(n, shape, rate);
}

// --- Phase C tier 3: actuarial closed-form ---

// [[Rcpp::export]]
Rcpp::NumericVector dfatigue(const Rcpp::NumericVector &x, const Rcpp::NumericVector &alpha,
                   const Rcpp::NumericVector &beta, const Rcpp::NumericVector &mu,
                   bool log = false) {
  return dist_impl::dfatigue(x, alpha, beta, mu, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pfatigue(const Rcpp::NumericVector &q, const Rcpp::NumericVector &alpha,
                   const Rcpp::NumericVector &beta, const Rcpp::NumericVector &mu,
                   bool lower_tail = true, bool log_p = false) {
  return dist_impl::pfatigue(q, alpha, beta, mu, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qfatigue(const Rcpp::NumericVector &p, const Rcpp::NumericVector &alpha,
                   const Rcpp::NumericVector &beta, const Rcpp::NumericVector &mu,
                   bool lower_tail = true, bool log_p = false) {
  return dist_impl::qfatigue(p, alpha, beta, mu, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rfatigue(int n, double alpha = 1.0, double beta = 1.0,
                   double mu = 0.0) {
  return dist_impl::rfatigue(n, alpha, beta, mu);
}

// [[Rcpp::export]]
Rcpp::NumericVector dgompertz(const Rcpp::NumericVector &x, const Rcpp::NumericVector &a,
                    const Rcpp::NumericVector &b, bool log = false) {
  return dist_impl::dgompertz(x, a, b, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pgompertz(const Rcpp::NumericVector &q, const Rcpp::NumericVector &a,
                    const Rcpp::NumericVector &b, bool lower_tail = true,
                    bool log_p = false) {
  return dist_impl::pgompertz(q, a, b, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qgompertz(const Rcpp::NumericVector &p, const Rcpp::NumericVector &a,
                    const Rcpp::NumericVector &b, bool lower_tail = true,
                    bool log_p = false) {
  return dist_impl::qgompertz(p, a, b, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rgompertz(int n, double a = 1.0, double b = 1.0) {
  return dist_impl::rgompertz(n, a, b);
}

// [[Rcpp::export]]
Rcpp::NumericVector dlgamma(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shapelog,
                  const Rcpp::NumericVector &ratelog, bool log = false) {
  return dist_impl::dlgamma(x, shapelog, ratelog, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector plgamma(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shapelog,
                  const Rcpp::NumericVector &ratelog, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::plgamma(q, shapelog, ratelog, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qlgamma(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shapelog,
                  const Rcpp::NumericVector &ratelog, bool lower_tail = true,
                  bool log_p = false) {
  return dist_impl::qlgamma(p, shapelog, ratelog, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rlgamma(int n, double shapelog, double ratelog) {
  return dist_impl::rlgamma(n, shapelog, ratelog);
}

// [[Rcpp::export]]
Rcpp::NumericVector dburr(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape1,
                const Rcpp::NumericVector &shape2, double scale = 1.0,
                bool log = false) {
  return dist_impl::dburr(x, shape1, shape2, scale, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pburr(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape1,
                const Rcpp::NumericVector &shape2, double scale = 1.0,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::pburr(q, shape1, shape2, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qburr(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape1,
                const Rcpp::NumericVector &shape2, double scale = 1.0,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::qburr(p, shape1, shape2, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rburr(int n, double shape1 = 1.0, double shape2 = 1.0,
                double scale = 1.0) {
  return dist_impl::rburr(n, shape1, shape2, scale);
}

// [[Rcpp::export]]
Rcpp::NumericVector dinvburr(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape1,
                   const Rcpp::NumericVector &shape2, double scale = 1.0,
                   bool log = false) {
  return dist_impl::dinvburr(x, shape1, shape2, scale, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pinvburr(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape1,
                   const Rcpp::NumericVector &shape2, double scale = 1.0,
                   bool lower_tail = true, bool log_p = false) {
  return dist_impl::pinvburr(q, shape1, shape2, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qinvburr(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape1,
                   const Rcpp::NumericVector &shape2, double scale = 1.0,
                   bool lower_tail = true, bool log_p = false) {
  return dist_impl::qinvburr(p, shape1, shape2, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rinvburr(int n, double shape1 = 1.0, double shape2 = 1.0,
                   double scale = 1.0) {
  return dist_impl::rinvburr(n, shape1, shape2, scale);
}

// [[Rcpp::export]]
Rcpp::NumericVector dparalogis(const Rcpp::NumericVector &x, const Rcpp::NumericVector &shape,
                     const Rcpp::NumericVector &scale, bool log = false) {
  return dist_impl::dparalogis(x, shape, scale, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pparalogis(const Rcpp::NumericVector &q, const Rcpp::NumericVector &shape,
                     const Rcpp::NumericVector &scale, bool lower_tail = true,
                     bool log_p = false) {
  return dist_impl::pparalogis(q, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qparalogis(const Rcpp::NumericVector &p, const Rcpp::NumericVector &shape,
                     const Rcpp::NumericVector &scale, bool lower_tail = true,
                     bool log_p = false) {
  return dist_impl::qparalogis(p, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rparalogis(int n, double shape = 1.0, double scale = 1.0) {
  return dist_impl::rparalogis(n, shape, scale);
}

// --- Phase C tier 4: special samplers ---

// [[Rcpp::export]]
Rcpp::NumericVector dlgser(const Rcpp::NumericVector &x, const Rcpp::NumericVector &theta,
                 bool log = false) {
  return dist_impl::dlgser(x, theta, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector plgser(const Rcpp::NumericVector &q, const Rcpp::NumericVector &theta,
                 bool lower_tail = true, bool log_p = false) {
  return dist_impl::plgser(q, theta, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qlgser(const Rcpp::NumericVector &p, const Rcpp::NumericVector &theta,
                 bool lower_tail = true, bool log_p = false) {
  return dist_impl::qlgser(p, theta, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rlgser(int n, double theta) {
  return dist_impl::rlgser(n, theta);
}

// [[Rcpp::export]]
Rcpp::NumericVector dzip(const Rcpp::NumericVector &x, const Rcpp::NumericVector &lambda,
               const Rcpp::NumericVector &pi, bool log = false) {
  return dist_impl::dzip(x, lambda, pi, log);
}
// [[Rcpp::export]]
Rcpp::NumericVector pzip(const Rcpp::NumericVector &q, const Rcpp::NumericVector &lambda,
               const Rcpp::NumericVector &pi, bool lower_tail = true,
               bool log_p = false) {
  return dist_impl::pzip(q, lambda, pi, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector qzip(const Rcpp::NumericVector &p, const Rcpp::NumericVector &lambda,
               const Rcpp::NumericVector &pi, bool lower_tail = true,
               bool log_p = false) {
  return dist_impl::qzip(p, lambda, pi, lower_tail, log_p);
}
// [[Rcpp::export]]
Rcpp::NumericVector rzip(int n, double lambda, double pi) {
  return dist_impl::rzip(n, lambda, pi);
}
