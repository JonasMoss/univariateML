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

