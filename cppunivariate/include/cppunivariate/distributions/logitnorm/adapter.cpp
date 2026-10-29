// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/logitnorm/mle.hpp"
#include <Rcpp.h>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mllogitnorm_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::logitnorm_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  if (!(res.sigma > 0.0) || !std::isfinite(res.sigma)) {
    Rcpp::stop("The logitnorm maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as scale shrinks to zero.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] =
          Rcpp::NumericVector::create(Rcpp::_["mu"] = res.mu,
                                      Rcpp::_["sigma"] = res.sigma),
      Rcpp::_["logLik"] = res.logLik);
}

#include "dpqr.h"
#include "cppunivariate/distributions/logitnorm/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::NumericVector dlogitnorm_(Rcpp::NumericVector x, Rcpp::NumericVector mu, Rcpp::NumericVector sigma, bool log=false) {
  return dist_impl::dpqr_apply({x,mu,sigma},[&](const std::vector<double> &v) {
    return cppunivariate::distributions::dlogitnorm(v[0],v[1],v[2],log);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector plogitnorm_(Rcpp::NumericVector q, Rcpp::NumericVector mu, Rcpp::NumericVector sigma, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({q,mu,sigma},[&](const std::vector<double> &v) {
    return cppunivariate::distributions::plogitnorm(v[0],v[1],v[2],lower_tail,log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector qlogitnorm_(Rcpp::NumericVector p, Rcpp::NumericVector mu, Rcpp::NumericVector sigma, bool lower_tail=true, bool log_p=false) {
  return dist_impl::dpqr_apply({p,mu,sigma},[&](const std::vector<double> &v) {
    return cppunivariate::distributions::qlogitnorm(v[0],v[1],v[2],lower_tail,log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector rlogitnorm_(int n, Rcpp::NumericVector mu, Rcpp::NumericVector sigma) {
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n),mu,sigma},[&](const std::vector<double> &v) {
    if (!cppunivariate::distributions::logitnorm_valid(v[1],v[2])) return R_NaN;
    const double z=R::rnorm(v[1],v[2]);
    return z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
  });
}
