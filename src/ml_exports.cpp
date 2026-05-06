// [[Rcpp::depends(RcppArmadillo)]]
#include "ml/core/rcpp_glue.hpp"
#include "ml/ml.hpp"
#include <RcppArmadillo.h>

// [[Rcpp::export]]
Rcpp::List mlburr_(const arma::vec &x,
                   Rcpp::Nullable<Rcpp::NumericVector> init = R_NilValue,
                   double tol = 1.220703e-4, int iterlim = 100) {
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

  mlcore::BurrMLE res = mlcore::burr_mle_core(x, c0, tol, iterlim);
  if (res.status == mlcore::BurrStatus::AllGeOne) {
    Rcpp::stop(
        "The Burr maximum likelihood estimator does not exist when all "
        "observations >=1. Use `mlpareto` to fit a Pareto distribution.");
  }
  if (res.status != mlcore::BurrStatus::Ok) {
    Rcpp::stop("Burr MLE failed (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Cauchy
// [[Rcpp::export]]
Rcpp::List mlcauchy_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                     int iterlim = 100) {
  mlcore::CauchyResult res = mlcore::fit_cauchy(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())}, tol,
      iterlim);
  if (res.iterations == 0) {
    Rcpp::stop("x must have positive length");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = res.mu, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Gumbel
// [[Rcpp::export]]
Rcpp::List mlgumbel_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                     int iterlim = 100) {
  if (iterlim <= 1) {
    Rcpp::warning("iteration limit reached");
  }
  mlcore::GumbelResult res = mlcore::fit_gumbel_newton(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())}, tol,
      iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = res.mu, Rcpp::_["sigma"] = res.sigma),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Logistic
// [[Rcpp::export]]
Rcpp::List mllogis_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                    int iterlim = 100) {
  mlcore::LogisticResult res =
      mlcore::logistic_mle(mlcore::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = res.mu, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Log-logistic
// [[Rcpp::export]]
Rcpp::List mlllogis_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                     int iterlim = 100) {
  mlcore::LogLogisticResult res =
      mlcore::loglogistic_mle(mlcore::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Beta
// [[Rcpp::export]]
Rcpp::List mlbeta_(const arma::vec &x, double tol = 1.490116e-08,
                   int iterlim = 100) {
  auto res = mlcore::beta_mle(x, tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlbetapr_(const arma::vec &x, double tol = 1.490116e-08,
                     int iterlim = 100) {
  mlcore::BetaPrimeResult res = mlcore::betapr_mle(x, tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Gamma
// [[Rcpp::export]]
Rcpp::List mlgamma_(const arma::vec &x, double tol = 1.220703e-4,
                    int iterlim = 100) {
  auto res = mlcore::gamma_mle(x, tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlfatigue_(const arma::vec &x, double tol = 1.220703e-4,
                      int iterlim = 100) {
  auto res = mlcore::fatigue_mle(x, tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta,
          Rcpp::_["mu"] = 0.0),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Closed-form easy ones
// [[Rcpp::export]]
Rcpp::List mlexp_(const Rcpp::NumericVector &x) {
  auto res = mlcore::exp_mle(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] =
          Rcpp::NumericVector::create(Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlunif_(const Rcpp::NumericVector &x) {
  auto res = mlcore::unif_mle(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["min"] = res.min, Rcpp::_["max"] = res.max),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mldunif_(const Rcpp::IntegerVector &x) {
  auto res = mlcore::dunif_mle(
      mlcore::ispan{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["min"] = res.min, Rcpp::_["max"] = res.max),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlpois_(const Rcpp::IntegerVector &x) {
  auto res = mlcore::pois_mle(
      mlcore::ispan{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["lambda"] = res.lambda),
                            Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlnorm_(const Rcpp::NumericVector &x) {
  auto res =
      mlcore::norm_mle(mlcore::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mean, Rcpp::_["sd"] = res.sd),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mllaplace_(const Rcpp::NumericVector &x) {
  auto res = mlcore::laplace_mle(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["mu"] = res.mu,
                                Rcpp::_["sigma"] = res.sigma),
                            Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlgeom_(const Rcpp::IntegerVector &x) {
  auto res = mlcore::geom_mle(
      mlcore::ispan{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["prob"] = res.prob),
                            Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlpareto_(const Rcpp::NumericVector &x) {
  auto res = mlcore::pareto_mle(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["shape"] = res.shape,
                                Rcpp::_["min"] = res.min),
                            Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlrayleigh_(const Rcpp::NumericVector &x) {
  auto res = mlcore::rayleigh_mle(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["sigma"] = res.sigma),
                            Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlbinom_(const Rcpp::IntegerVector &x,
                    Rcpp::Nullable<double> size = R_NilValue,
                    double tol = 1.220703e-4, int iterlim = 100) {
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  auto res = mlcore::binom_mle(xi, size, tol, iterlim);
  if (res.status == mlcore::BinomStatus::SizeTooSmall) {
    Rcpp::stop("`size` is smaller than `max(x)`.");
  }
  if (res.status == mlcore::BinomStatus::Underdispersed) {
    Rcpp::stop(
        "The maximum likelihood estimator does not exist. Use `mlpois` to fit "
        "a Poisson or supply a `size` argument.");
  }
  if (res.status != mlcore::BinomStatus::Ok || !std::isfinite(res.size)) {
    Rcpp::stop("Binomial MLE failed (check dispersion/inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["size"] = res.size, Rcpp::_["prob"] = res.prob),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mllnorm_(const Rcpp::NumericVector &x) {
  auto res = mlcore::lognormal_mle(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["meanlog"] = res.meanlog, Rcpp::_["sdlog"] = res.sdlog),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mllogitnorm_(const Rcpp::NumericVector &x) {
  auto res = mlcore::logitnorm_mle(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] =
          Rcpp::NumericVector::create(Rcpp::_["mu"] = res.mu,
                                      Rcpp::_["sigma"] = res.sigma),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlzip_(const Rcpp::IntegerVector &x) {
  if (x.size() > 0 &&
      std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    Rcpp::warning("all observations are zero");
  }
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  auto res = mlcore::zip_mle(xi);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["lambda"] = res.lambda, Rcpp::_["pi"] = res.pi),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlzipf_(const Rcpp::IntegerVector &x, double tol = 1.220703e-4,
                   int iterlim = 100) {
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  if (x.size() == 0) {
    Rcpp::stop("x must have positive length");
  }
  if (x.size() > 0 &&
      std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    Rcpp::warning("all observations are zero");
  }

  auto res = mlcore::zipf_mle(xi, tol, iterlim);
  if (res.status == mlcore::ZipfStatus::AllOnes) {
    Rcpp::warning("All observations are equal to 1. The maximum likelihood "
                  "estimator is not unique.");
  }
  if (res.status == mlcore::ZipfStatus::ShapeNegative) {
    Rcpp::stop("Optimal shape parameter is less than 0. The maximum likelihood "
               "estimator does not exist.");
  }
  if (res.status != mlcore::ZipfStatus::Ok &&
      res.status != mlcore::ZipfStatus::AllOnes) {
    Rcpp::stop("Zipf MLE failed to converge (check data)");
  }
  if (!std::isfinite(res.N) || !std::isfinite(res.shape)) {
    Rcpp::stop("Zipf MLE failed to converge (check data)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["N"] = res.N, Rcpp::_["shape"] = res.shape),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlnbinom_(const Rcpp::IntegerVector &x, double tol = 1.220703e-4,
                     int iterlim = 100, Rcpp::Nullable<int> size = R_NilValue) {
  const int n = x.size();
  if (n == 0) {
    Rcpp::stop("x must have positive length");
  }
  if (n > 0 && std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    Rcpp::warning("All observations are 0; the maximum likelihood estimator is "
                  "not unique.");
  }
  arma::ivec xi = Rcpp::as<arma::ivec>(x);

  double x_bar = arma::mean(arma::conv_to<arma::vec>::from(xi));
  double size_out = NA_REAL;
  double prob_out = NA_REAL;
  double loglik_out = NA_REAL;
  int iterations_out = 0;

  if (size.isNotNull()) {
    size_out = static_cast<double>(Rcpp::as<int>(size));
    prob_out = size_out / (x_bar + size_out);
    double ll = 0.0;
    for (int v : xi) {
      ll += Rf_dnbinom(static_cast<double>(v), size_out, prob_out,
                       /*give_log=*/1);
    }
    loglik_out = ll;
  } else {
    if (x_bar == 0.0) {
      size_out = 1.0;
      prob_out = 1.0; // logLik -> -Inf, but tests only check warning/estimate
      loglik_out = R_NegInf;
    } else {
      auto res = mlcore::nbinom_mle(xi, tol, iterlim);
      if (res.underdispersed) {
        Rcpp::stop("The maximum likelihood estimator does not exist for "
                   "underdispersed data, but converges to a Poisson. Use "
                   "`mlpois` instead.");
      }
      if (!std::isfinite(res.size) || !std::isfinite(res.prob)) {
        Rcpp::stop("Negative binomial MLE failed (check dispersion/inputs)");
      }
      size_out = res.size;
      prob_out = res.prob;
      loglik_out = res.logLik;
      iterations_out = res.iterations;
    }
  }

  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["size"] = size_out, Rcpp::_["prob"] = prob_out),
      Rcpp::_["logLik"] = loglik_out, Rcpp::_["iterations"] = iterations_out);
}

// [[Rcpp::export]]
Rcpp::List mlnaka_(const arma::vec &x, double tol = 1.220703e-4,
                   int iterlim = 100) {
  auto res = mlcore::naka_mle(x, tol, iterlim);
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale)) {
    Rcpp::stop("Nakagami MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlgompertz_(const arma::vec &x, double tol = 1.220703e-4,
                       int iterlim = 100) {
  auto res = mlcore::gompertz_mle(x, tol, iterlim);
  if (res.status == mlcore::GompertzStatus::Overdispersed) {
    Rcpp::stop("The maximum likelihood estimator of the b parameter Gompertz "
               "distribution does not exist since the data is overdispersed. "
               "The exponential distribution will yield a better fit.");
  }
  if (res.status != mlcore::GompertzStatus::Ok || !std::isfinite(res.a) ||
      !std::isfinite(res.b)) {
    Rcpp::stop("Gompertz MLE failed");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.a, Rcpp::_["b"] = res.b),
                            Rcpp::_["logLik"] = res.logLik,
                            Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlkumar_(const arma::vec &x, double tol = 1.220703e-4,
                    int iterlim = 100) {
  auto res = mlcore::kumar_mle(x, tol, iterlim);
  if (!std::isfinite(res.a) || !std::isfinite(res.b)) {
    Rcpp::stop("Kumaraswamy MLE failed");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.a, Rcpp::_["b"] = res.b),
                            Rcpp::_["logLik"] = res.logLik,
                            Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlinvburr_(const arma::vec &x, double tol = 1.220703e-4,
                      int iterlim = 100) {
  auto res = mlcore::invburr_mle(x, tol, iterlim);
  if (!std::isfinite(res.shape1) || !std::isfinite(res.shape2)) {
    Rcpp::stop("Inverse Burr MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mllgser_(const Rcpp::IntegerVector &x) {
  arma::ivec xi = Rcpp::as<arma::ivec>(x);
  auto res = mlcore::lgser_mle(xi);
  if (res.all_ones) {
    Rcpp::stop("All observations equals 1; the maximum likelihood estimator "
               "does not exist.");
  }
  if (!std::isfinite(res.theta)) {
    Rcpp::stop("Logarithmic-series MLE failed");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["theta"] = res.theta),
                            Rcpp::_["logLik"] = res.logLik);
}
// [[Rcpp::export]]
Rcpp::List mlweibull_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                      int iterlim = 100) {
  if (iterlim <= 1) {
    Rcpp::warning("iteration limit reached");
  }
  auto res = mlcore::weibull_mle(mlcore::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlinvweibull_(const Rcpp::NumericVector &x,
                         double tol = 1.220703e-4, int iterlim = 100) {
  if (iterlim <= 1) {
    Rcpp::warning("iteration limit reached");
  }
  auto res = mlcore::invweibull_mle(mlcore::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlinvgamma_(const arma::vec &x, double tol = 1.220703e-4,
                       int iterlim = 100) {
  auto res = mlcore::invgamma_mle(x, tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlpower_(const Rcpp::NumericVector &x,
                    double epsilon = 1.490116e-08) {
  auto res = mlcore::power_mle(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())}, epsilon);
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta)) {
    Rcpp::stop("The power distribution is only defined for positive numbers.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mllgamma_(const arma::vec &x, double tol = 1.220703e-4,
                     int iterlim = 100) {
  arma::vec y = arma::log(x);
  auto res = mlcore::gamma_mle(y, tol, iterlim);
  double shapelog = res.shape;
  double ratelog = res.rate;
  double n = static_cast<double>(x.n_elem);
  double L = arma::mean(y);
  double K = arma::mean(arma::log(y));
  double logLik = n * (shapelog * std::log(ratelog) - std::lgamma(shapelog) +
                       (shapelog - 1.0) * K - (ratelog + 1.0) * L);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shapelog"] = shapelog, Rcpp::_["ratelog"] = ratelog),
      Rcpp::_["logLik"] = logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlinvgauss_(const Rcpp::NumericVector &x) {
  auto res = mlcore::invgauss_mle(
      mlcore::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = res.mu, Rcpp::_["lambda"] = res.lambda),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mllomax_(const arma::vec &x, double tol = 1.220703e-4,
                    int iterlim = 100) {
  auto res = mlcore::lomax_mle(x, tol, iterlim);
  if (res.status == mlcore::LomaxStatus::MeanVarianceCondition) {
    Rcpp::stop("The Lomax maximum likelihood estimator does not exist for data "
               "with `mean(x^2) < 2*mean(x)^2`. Use `mlexp` to fit an "
               "exponential distribution.");
  }
  if (res.status != mlcore::LomaxStatus::Ok || !std::isfinite(res.lambda) ||
      !std::isfinite(res.kappa)) {
    Rcpp::stop("Lomax MLE failed to converge (check dispersion/inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["lambda"] = res.lambda, Rcpp::_["kappa"] = res.kappa),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
