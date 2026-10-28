#include "as_span.h"
#include "cppunivariate/cppunivariate.hpp"
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List mlburr_(const Rcpp::NumericVector &x,
                   Rcpp::Nullable<Rcpp::NumericVector> init = R_NilValue,
                   double tol = 1.220703e-4, int iterlim = 100) {
  const std::size_t n = static_cast<std::size_t>(x.size());

  // Preserve the actionable Pareto diagnostic before computing a start.
  bool all_ge_one = n > 0;
  for (std::size_t i = 0; i < n; ++i) {
    if (x[i] < 1.0) { all_ge_one = false; break; }
  }
  if (all_ge_one) {
    Rcpp::stop(
        "The Burr maximum likelihood estimator does not exist when all "
        "observations >=1. Use `mlpareto` to fit a Pareto distribution.");
  }

  double c0;
  if (init.isNotNull()) {
    Rcpp::NumericVector iv(init);
    if (iv.size() < 1 || Rcpp::NumericVector::is_na(iv[0]) || iv[0] <= 0) {
      Rcpp::stop("`init` must be a length-1 numeric vector with a positive c");
    }
    c0 = iv[0];
  } else {
    c0 = cppunivariate::burr_mle_start(dist_impl::as_span(x));
    if (!std::isfinite(c0) || c0 <= 0.0) {
      Rcpp::stop("Failed to get a valid initial c for Burr MLE");
    }
  }

  cppunivariate::BurrMLE res =
      cppunivariate::burr_mle_core(dist_impl::as_span(x), c0, tol, iterlim);
  if (res.status == cppunivariate::BurrStatus::AllGeOne) {
    Rcpp::stop(
        "The Burr maximum likelihood estimator does not exist when all "
        "observations >=1. Use `mlpareto` to fit a Pareto distribution.");
  }
  if (res.status != cppunivariate::BurrStatus::Ok) {
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
  cppunivariate::CauchyResult res = cppunivariate::fit_cauchy(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, tol,
      iterlim);
  if (res.status == cppunivariate::CauchyStatus::NoMLE)
    Rcpp::stop("Cauchy MLE does not exist: need n >= 3 and no value repeated n/2 or more times");
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    Rcpp::stop("Cauchy MLE failed: non-finite or invalid parameters");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["location"] = res.mu, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Gumbel
// [[Rcpp::export]]
Rcpp::List mlgumbel_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                     int iterlim = 100) {
  if (iterlim <= 1) {
    Rcpp::warning("iteration limit reached");
  }
  cppunivariate::GumbelResult res = cppunivariate::fit_gumbel_newton(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, tol,
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
  cppunivariate::LogisticResult res =
      cppunivariate::logistic_mle(dist_impl::as_span(x), tol, iterlim);
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    Rcpp::stop("Logistic MLE failed: non-finite or invalid parameters");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["location"] = res.mu, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Log-logistic
// [[Rcpp::export]]
Rcpp::List mlllogis_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                     int iterlim = 100) {
  cppunivariate::LogLogisticResult res =
      cppunivariate::loglogistic_mle(dist_impl::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Beta
// [[Rcpp::export]]
Rcpp::List mlbeta_(const Rcpp::NumericVector &x, double tol = 1.490116e-08,
                   int iterlim = 100) {
  auto res = cppunivariate::beta_mle(dist_impl::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.alpha, Rcpp::_["shape2"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlbetapr_(const Rcpp::NumericVector &x, double tol = 1.490116e-08,
                     int iterlim = 100) {
  cppunivariate::BetaPrimeResult res =
      cppunivariate::betapr_mle(dist_impl::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.alpha, Rcpp::_["shape2"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Gamma
// [[Rcpp::export]]
Rcpp::List mlgamma_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                    int iterlim = 100) {
  auto res = cppunivariate::gamma_mle(dist_impl::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlfatigue_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                      int iterlim = 100) {
  auto res = cppunivariate::fatigue_mle(dist_impl::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta,
          Rcpp::_["mu"] = 0.0),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// Closed-form easy ones
// [[Rcpp::export]]
Rcpp::List mlexp_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::exp_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] =
          Rcpp::NumericVector::create(Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlunif_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::unif_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["min"] = res.min, Rcpp::_["max"] = res.max),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mldunif_(const Rcpp::IntegerVector &x) {
  auto res = cppunivariate::dunif_mle(
      cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["min"] = res.min, Rcpp::_["max"] = res.max),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlpois_(const Rcpp::IntegerVector &x) {
  auto res = cppunivariate::pois_mle(
      cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["lambda"] = res.lambda),
                            Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlnorm_(const Rcpp::NumericVector &x) {
  auto res =
      cppunivariate::norm_mle(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mean, Rcpp::_["sd"] = res.sd),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mllaplace_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::laplace_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["mu"] = res.mu,
                                Rcpp::_["sigma"] = res.sigma),
                            Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlgeom_(const Rcpp::IntegerVector &x) {
  auto res = cppunivariate::geom_mle(
      cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["prob"] = res.prob),
                            Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlpareto_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::pareto_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.shape,
                                Rcpp::_["b"] = res.min),
                            Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlrayleigh_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::rayleigh_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["sigma"] = res.sigma),
                            Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlbinom_(const Rcpp::IntegerVector &x,
                    Rcpp::Nullable<double> size = R_NilValue,
                    double tol = 1.220703e-4, int iterlim = 100) {
  const double size_or_nan = size.isNotNull()
                                 ? Rcpp::as<double>(size)
                                 : std::numeric_limits<double>::quiet_NaN();
  auto res = cppunivariate::binom_mle(dist_impl::as_ispan(x), size_or_nan, tol, iterlim);
  if (res.status == cppunivariate::BinomStatus::SizeTooSmall) {
    Rcpp::stop("`size` is smaller than `max(x)`.");
  }
  if (res.status == cppunivariate::BinomStatus::Underdispersed) {
    Rcpp::stop(
        "The maximum likelihood estimator does not exist. Use `mlpois` to fit "
        "a Poisson or supply a `size` argument.");
  }
  if (res.status != cppunivariate::BinomStatus::Ok || !std::isfinite(res.size)) {
    Rcpp::stop("Binomial MLE failed (check dispersion/inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["size"] = res.size, Rcpp::_["prob"] = res.prob),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mllnorm_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::lognormal_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["meanlog"] = res.meanlog, Rcpp::_["sdlog"] = res.sdlog),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mllogitnorm_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::logitnorm_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
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
  auto res = cppunivariate::zip_mle(dist_impl::as_ispan(x));
  if (res.status == cppunivariate::ZipStatus::InvalidInput)
    Rcpp::stop("Zero-inflated Poisson MLE requires nonempty nonnegative count data.");
  if (res.status != cppunivariate::ZipStatus::Ok)
    Rcpp::stop("Zero-inflated Poisson MLE failed to produce a finite valid fit.");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["lambda"] = res.lambda, Rcpp::_["pi"] = res.pi),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlzipf_(const Rcpp::IntegerVector &x, double tol = 1.220703e-4,
                   int iterlim = 100) {
  if (x.size() == 0) {
    Rcpp::stop("x must have positive length");
  }
  if (x.size() > 0 &&
      std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    Rcpp::warning("all observations are zero");
  }

  auto res = cppunivariate::zipf_mle(dist_impl::as_ispan(x), tol, iterlim);
  if (res.status == cppunivariate::ZipfStatus::AllOnes) {
    Rcpp::warning("All observations are equal to 1. The maximum likelihood "
                  "estimator is not unique.");
  }
  if (res.status == cppunivariate::ZipfStatus::ShapeNegative) {
    Rcpp::stop("Optimal shape parameter is less than 0. The maximum likelihood "
               "estimator does not exist.");
  }
  if (res.status != cppunivariate::ZipfStatus::Ok &&
      res.status != cppunivariate::ZipfStatus::AllOnes) {
    Rcpp::stop("Zipf MLE failed to converge (check data)");
  }
  if (!std::isfinite(res.N) || !std::isfinite(res.shape)) {
    Rcpp::stop("Zipf MLE failed to converge (check data)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["N"] = res.N, Rcpp::_["s"] = res.shape),
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
  double sum_x = 0.0;
  for (int v : x) sum_x += static_cast<double>(v);
  const double x_bar = sum_x / static_cast<double>(x.size());

  double size_out = NA_REAL;
  double prob_out = NA_REAL;
  double loglik_out = NA_REAL;
  int iterations_out = 0;

  if (size.isNotNull()) {
    size_out = static_cast<double>(Rcpp::as<int>(size));
    prob_out = size_out / (x_bar + size_out);
    double ll = 0.0;
    for (int v : x) {
      ll += cppunivariate::special::dnbinom(static_cast<double>(v), size_out, prob_out,
                                     /*log=*/true);
    }
    loglik_out = ll;
  } else {
    if (x_bar == 0.0) {
      size_out = 1.0;
      prob_out = 1.0;
      loglik_out = 0.0;
    } else {
      auto res = cppunivariate::nbinom_mle(dist_impl::as_ispan(x), tol, iterlim);
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
Rcpp::List mlnaka_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                   int iterlim = 100) {
  auto res = cppunivariate::naka_mle(dist_impl::as_span(x), tol, iterlim);
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale)) {
    Rcpp::stop("Nakagami MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlgompertz_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                       int iterlim = 100) {
  auto res = cppunivariate::gompertz_mle(dist_impl::as_span(x), tol, iterlim);
  if (res.status == cppunivariate::GompertzStatus::Overdispersed) {
    Rcpp::stop("The maximum likelihood estimator of the b parameter Gompertz "
               "distribution does not exist since the data is overdispersed. "
               "The exponential distribution will yield a better fit.");
  }
  if (res.status != cppunivariate::GompertzStatus::Ok || !std::isfinite(res.a) ||
      !std::isfinite(res.b)) {
    Rcpp::stop("Gompertz MLE failed");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.a, Rcpp::_["b"] = res.b),
                            Rcpp::_["logLik"] = res.logLik,
                            Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlkumar_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                    int iterlim = 100) {
  auto res = cppunivariate::kumar_mle(dist_impl::as_span(x), tol, iterlim);
  if (!std::isfinite(res.a) || !std::isfinite(res.b)) {
    Rcpp::stop("Kumaraswamy MLE failed");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["a"] = res.a, Rcpp::_["b"] = res.b),
                            Rcpp::_["logLik"] = res.logLik,
                            Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlinvburr_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                      int iterlim = 100) {
  auto res = cppunivariate::invburr_mle(dist_impl::as_span(x), tol, iterlim);
  if (res.status == cppunivariate::InvBurrStatus::AllLeOne) {
    Rcpp::stop(
        "The inverse Burr maximum likelihood estimator does not exist when all "
        "observations <=1. Use `mlpower` to fit a power distribution.");
  }
  if (res.status != cppunivariate::InvBurrStatus::Ok) {
    Rcpp::stop("Inverse Burr MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mllgser_(const Rcpp::IntegerVector &x) {
  auto res = cppunivariate::lgser_mle(dist_impl::as_ispan(x));
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
  auto res = cppunivariate::weibull_mle(dist_impl::as_span(x), tol, iterlim);
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
  auto res = cppunivariate::invweibull_mle(dist_impl::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlinvgamma_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                       int iterlim = 100) {
  auto res = cppunivariate::invgamma_mle(dist_impl::as_span(x), tol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.shape, Rcpp::_["beta"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlpower_(const Rcpp::NumericVector &x,
                    double epsilon = 1.490116e-08) {
  auto res = cppunivariate::power_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, epsilon);
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta)) {
    Rcpp::stop("The power distribution is only defined for positive numbers.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.alpha, Rcpp::_["beta"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mllgamma_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                     int iterlim = 100) {
  const std::size_t n = static_cast<std::size_t>(x.size());
  std::vector<double> y(n);
  double sum_y = 0.0, sum_log_y = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    y[i] = std::log(x[i]);
    sum_y += y[i];
    sum_log_y += std::log(y[i]);
  }
  auto res =
      cppunivariate::gamma_mle(cppunivariate::span{y.data(), n}, tol, iterlim);
  const double shapelog = res.shape;
  const double ratelog = res.rate;
  const double dn = static_cast<double>(n);
  const double L = sum_y / dn;
  const double K = sum_log_y / dn;
  const double logLik =
      dn * (shapelog * std::log(ratelog) - std::lgamma(shapelog) +
            (shapelog - 1.0) * K - (ratelog + 1.0) * L);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shapelog"] = shapelog, Rcpp::_["ratelog"] = ratelog),
      Rcpp::_["logLik"] = logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlinvgauss_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::invgauss_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["shape"] = res.lambda),
      Rcpp::_["logLik"] = res.logLik);
}

// [[Rcpp::export]]
Rcpp::List mlsged_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                   int iterlim = 200) {
  auto res = cppunivariate::sged_mle(dist_impl::as_span(x), tol, iterlim);
  if (res.status == cppunivariate::SgedStatus::InvalidStart) {
    Rcpp::stop("Skew GED MLE requires at least 2 finite observations with "
               "positive variance.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu) || !std::isfinite(res.xi)) {
    Rcpp::stop("Skew GED MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu, Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlsstd_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                   int iterlim = 200) {
  auto res = cppunivariate::sstd_mle(dist_impl::as_span(x), tol, iterlim);
  if (res.status == cppunivariate::SstdStatus::InvalidStart) {
    Rcpp::stop("Skew Student-t MLE requires at least 2 finite observations "
               "with positive variance.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu) || !std::isfinite(res.xi)) {
    Rcpp::stop("Skew Student-t MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu, Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlsnorm_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                    int iterlim = 200) {
  auto res = cppunivariate::snorm_mle(dist_impl::as_span(x), tol, iterlim);
  if (res.status == cppunivariate::SnormStatus::InvalidStart) {
    Rcpp::stop("Skew-normal MLE requires at least 2 finite observations with "
               "positive variance.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.xi)) {
    Rcpp::stop("Skew-normal MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlged_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                  int iterlim = 200) {
  auto res = cppunivariate::ged_mle(dist_impl::as_span(x), tol, iterlim);
  if (res.status == cppunivariate::GedStatus::InvalidStart) {
    Rcpp::stop("GED MLE requires at least 2 finite observations with positive "
               "variance.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu)) {
    Rcpp::stop("GED MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

// [[Rcpp::export]]
Rcpp::List mlstd_(const Rcpp::NumericVector &x, double tol = 1.220703e-4,
                  int iterlim = 200) {
  auto res = cppunivariate::student_t_mle(dist_impl::as_span(x), tol, iterlim);
  if (res.status == cppunivariate::StudentTStatus::NormalBoundary) {
    Rcpp::stop("The Student-t likelihood is maximised as nu -> Inf, i.e. by the "
               "normal distribution. Use `mlnorm` to fit this model.");
  }
  if (res.status == cppunivariate::StudentTStatus::DfTwoBoundary) {
    Rcpp::stop("The Student-t likelihood is maximised as nu -> 2 (infinite "
               "variance); the MLE does not exist in the mean/sd parameterisation.");
  }
  if (res.status == cppunivariate::StudentTStatus::InvalidStart) {
    Rcpp::stop("Student-t MLE requires at least 2 finite observations with "
               "positive variance.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu)) {
    Rcpp::stop("Student-t MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}

