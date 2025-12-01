// [[Rcpp::depends(RcppArmadillo)]]
#include "ml_common.hpp"
#include "ml_core.hpp"
#include <RcppArmadillo.h>

// Burr
// [[Rcpp::export]]
Rcpp::NumericVector burr_lmom(double mu, double sigma2) {
  auto res = mlcore::burr_lmom_core(mu, sigma2, true);
  return Rcpp::NumericVector::create(Rcpp::_["k"] = res.k, Rcpp::_["c"] = res.c,
                                     Rcpp::_["iter"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::NumericVector
mlburr_rcpp(const arma::vec &x,
            Rcpp::Nullable<Rcpp::NumericVector> init = R_NilValue,
            double tol = 1.220703e-4, int maxit = 100) {
  const double nd = static_cast<double>(x.n_elem);
  arma::vec log_x = arma::log(x);
  double m1 = arma::accu(log_x) / nd;
  double m2 = (arma::dot(log_x, log_x) / nd) - m1 * m1;

  double c0;
  if (init.isNotNull()) {
    Rcpp::NumericVector iv(init);
    if (iv.size() < 1 || Rcpp::NumericVector::is_na(iv[0]) || iv[0] <= 0) {
      Rcpp::stop("`init` must be a length-1 numeric vector with a positive c");
    }
    c0 = iv[0];
  } else {
    c0 = mlcore::burr_lmom_core(m1, m2, true).c;
    if (Rcpp::NumericVector::is_na(c0) || c0 <= 0.0) {
      Rcpp::stop("Failed to get a valid initial c from burr_lmom()");
    }
  }

  mlcore::BurrMLE res = mlcore::burr_mle_core(x, c0, tol, maxit);
  return Rcpp::NumericVector::create(Rcpp::_["shape1"] = res.shape1,
                                     Rcpp::_["shape2"] = res.shape2,
                                     Rcpp::_["iter"] = res.iterations);
}

// Cauchy
// [[Rcpp::export]]
Rcpp::NumericVector mlcauchy_rcpp(const arma::vec &x,
                                  double tol = 1.220703e-4,
                                  int maxit = 100) {
  mlcore::CauchyResult res = mlcore::fit_cauchy(x, tol, maxit);
  if (res.iterations == 0) {
    Rcpp::stop("x must have positive length");
  }
  return Rcpp::NumericVector::create(
    Rcpp::_["mu"]    = res.mu,
    Rcpp::_["scale"] = res.scale,
    Rcpp::_["iterations"] = res.iterations
  );
}

// Gumbel
// [[Rcpp::export]]
Rcpp::NumericVector ml_gumbel_rcpp(const arma::vec &x, double tol = 1.220703e-4,
                                   int maxit = 100) {
  mlcore::GumbelResult res = mlcore::fit_gumbel_newton(x, tol, maxit);
  Rcpp::NumericVector out(3);
  out[0] = res.mu;
  out[1] = res.sigma;
  out[2] = res.iterations;
  out.names() = Rcpp::CharacterVector::create("mu", "sigma", "iterations");
  return out;
}

// Logistic
// [[Rcpp::export]]
Rcpp::List mllogis_fast(const arma::vec &x, double tol = 1.220703e-4,
                        int maxit = 100) {
  mlcore::LogisticResult res = mlcore::logistic_mle(x, tol, maxit);
  return Rcpp::List::create(
    Rcpp::_["estimates"]  = Rcpp::NumericVector::create(
      Rcpp::_["mu"]    = res.mu,
      Rcpp::_["scale"] = res.scale
    ),
    Rcpp::_["iterations"] = res.iterations
  );
}

// inverse digamma utility
// [[Rcpp::export]]
Rcpp::NumericVector inverse_digamma(Rcpp::NumericVector x) {
  Rcpp::NumericVector out(x.size());
  for (int i = 0; i < x.size(); ++i) {
    out[i] = mlcore::inverse_digamma_scalar(x[i]);
  }
  return out;
}

// Beta
// [[Rcpp::export]]
Rcpp::List mlbeta_rcpp(const arma::vec& x, double tol = 1.220703e-4, int maxit = 100) {
  auto res = mlcore::beta_mle(x, tol, maxit);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["alpha"] = res.alpha,
      Rcpp::_["beta"]  = res.beta
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mlbetapr_rcpp(const arma::vec& x, double tol = 1.220703e-4, int maxit = 100) {
  // transform to beta on y = x/(x+1)
  arma::vec y = x / (x + 1.0);
  return mlbeta_rcpp(y, tol, maxit);
}

// Gamma
// [[Rcpp::export]]
Rcpp::List mlgamma_rcpp(const arma::vec& x, double tol = 1.220703e-4, int maxit = 100) {
  auto res = mlcore::gamma_mle(x, tol, maxit);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["shape"] = res.shape,
      Rcpp::_["rate"]  = res.rate
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// Closed-form easy ones
// [[Rcpp::export]]
Rcpp::List mlexp_rcpp(const arma::vec& x) {
  auto res = mlcore::exp_mle(x);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["rate"] = res.estimates[0]),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mlunif_rcpp(const arma::vec& x) {
  auto res = mlcore::unif_mle(x);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["min"] = res.estimates[0],
                                                       Rcpp::_["max"] = res.estimates[1]),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mlpois_rcpp(const Rcpp::IntegerVector& x) {
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  auto res = mlcore::pois_mle(xi);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["lambda"] = res.estimates[0]),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mlnorm_rcpp(const arma::vec& x) {
  auto res = mlcore::norm_mle(x);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["mean"] = res.estimates[0],
                                                       Rcpp::_["sd"]   = res.estimates[1]),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mllaplace_rcpp(const arma::vec& x) {
  auto res = mlcore::laplace_mle(x);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["mu"] = res.estimates[0],
                                                       Rcpp::_["sigma"] = res.estimates[1]),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mlgeom_rcpp(const Rcpp::IntegerVector& x) {
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  auto res = mlcore::geom_mle(xi);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["prob"] = res.estimates[0]),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mlpareto_rcpp(const arma::vec& x) {
  auto res = mlcore::pareto_mle(x);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["shape"] = res.estimates[0],
                                                       Rcpp::_["min"] = res.estimates[1]),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mlrayleigh_rcpp(const arma::vec& x) {
  auto res = mlcore::rayleigh_mle(x);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["sigma"] = res.estimates[0]),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mlbinom_rcpp(const Rcpp::IntegerVector& x,
                        double tol = 1.220703e-4,
                        int maxit = 100) {
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  auto res = mlcore::binom_mle(xi, tol, maxit);
  if (res.iterations == 0 || !std::isfinite(res.size)) {
    Rcpp::stop("Binomial MLE failed (check dispersion/inputs)");
  }
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["size"] = res.size,
                                                       Rcpp::_["prob"] = res.prob),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mllnorm_rcpp(const arma::vec& x) {
  auto res = mlcore::lognormal_mle(x);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["meanlog"] = res.estimates[0],
                                                       Rcpp::_["sdlog"]  = res.estimates[1]),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mllogitnorm_rcpp(const arma::vec& x) {
  auto res = mlcore::logitnorm_mle(x);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["mu"] = res.estimates[0],
                                                       Rcpp::_["sigma"] = res.estimates[1]),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mlzip_rcpp(const Rcpp::IntegerVector& x) {
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  auto res = mlcore::zip_mle(xi);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["lambda"] = res.lambda,
      Rcpp::_["pi"]     = res.pi
    ),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mlzipf_rcpp(const Rcpp::IntegerVector& x,
                       double tol = 1.220703e-4,
                       int maxit = 100) {
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  auto res = mlcore::zipf_mle(xi, tol, maxit);
  if (!std::isfinite(res.shape)) {
    Rcpp::stop("Zipf MLE failed to converge (check data)");
  }
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["N"]     = res.N,
      Rcpp::_["shape"] = res.shape
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mlnbinom_rcpp(const Rcpp::IntegerVector& x,
                         double tol = 1.220703e-4,
                         int maxit = 100) {
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  auto res = mlcore::nbinom_mle(xi, tol, maxit);
  if (!std::isfinite(res.size) || !std::isfinite(res.prob)) {
    Rcpp::stop("Negative binomial MLE failed (check dispersion/inputs)");
  }
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["size"] = res.size,
      Rcpp::_["prob"] = res.prob
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mlnaka_rcpp(const arma::vec& x,
                       double tol = 1.220703e-4,
                       int maxit = 100) {
  auto res = mlcore::naka_mle(x, tol, maxit);
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale)) {
    Rcpp::stop("Nakagami MLE failed");
  }
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["shape"] = res.shape,
      Rcpp::_["scale"] = res.scale
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mlgompertz_rcpp(const arma::vec& x,
                           double tol = 1.220703e-4,
                           int maxit = 100) {
  auto res = mlcore::gompertz_mle(x, tol, maxit);
  if (!std::isfinite(res.a) || !std::isfinite(res.b)) {
    Rcpp::stop("Gompertz MLE failed");
  }
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["a"] = res.a,
      Rcpp::_["b"] = res.b
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mlkumar_rcpp(const arma::vec& x,
                        double tol = 1.220703e-4,
                        int maxit = 100) {
  auto res = mlcore::kumar_mle(x, tol, maxit);
  if (!std::isfinite(res.a) || !std::isfinite(res.b)) {
    Rcpp::stop("Kumaraswamy MLE failed");
  }
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["a"] = res.a,
      Rcpp::_["b"] = res.b
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mlinvburr_rcpp(const arma::vec& x,
                          double tol = 1.220703e-4,
                          int maxit = 100) {
  auto res = mlcore::invburr_mle(x, tol, maxit);
  if (!std::isfinite(res.shape1) || !std::isfinite(res.shape2)) {
    Rcpp::stop("Inverse Burr MLE failed");
  }
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["shape1"] = res.shape1,
      Rcpp::_["shape2"] = res.shape2
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mllgser_rcpp(const Rcpp::IntegerVector& x) {
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  auto res = mlcore::lgser_mle(xi);
  if (!std::isfinite(res.theta)) {
    Rcpp::stop("Logarithmic-series MLE failed");
  }
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(Rcpp::_["theta"] = res.theta),
    Rcpp::_["logLik"] = res.logLik
  );
}
// [[Rcpp::export]]
Rcpp::List mlweibull_rcpp(const arma::vec& x,
                          double tol = 1.220703e-4,
                          int maxit = 100) {
  auto res = mlcore::weibull_mle(x, tol, maxit);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["shape"] = res.shape,
      Rcpp::_["scale"] = res.scale
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mlinvweibull_rcpp(const arma::vec& x,
                             double tol = 1.220703e-4,
                             int maxit = 100) {
  auto res = mlcore::invweibull_mle(x, tol, maxit);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["shape"] = res.shape,
      Rcpp::_["rate"]  = res.rate
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mlinvgamma_rcpp(const arma::vec& x,
                           double tol = 1.220703e-4,
                           int maxit = 100) {
  auto res = mlcore::invgamma_mle(x, tol, maxit);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["shape"] = res.shape,
      Rcpp::_["rate"]  = res.rate
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}

// [[Rcpp::export]]
Rcpp::List mlinvgauss_rcpp(const arma::vec& x) {
  auto res = mlcore::invgauss_mle(x);
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["mu"]     = res.mu,
      Rcpp::_["lambda"] = res.lambda
    ),
    Rcpp::_["logLik"] = res.logLik
  );
}

// [[Rcpp::export]]
Rcpp::List mllomax_rcpp(const arma::vec& x,
                        double tol = 1.220703e-4,
                        int maxit = 100) {
  auto res = mlcore::lomax_mle(x, tol, maxit);
  if (!std::isfinite(res.lambda) || !std::isfinite(res.kappa)) {
    Rcpp::stop("Lomax MLE failed to converge (check dispersion/inputs)");
  }
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["lambda"] = res.lambda,
      Rcpp::_["kappa"]  = res.kappa
    ),
    Rcpp::_["logLik"] = res.logLik,
    Rcpp::_["iterations"] = res.iterations
  );
}
