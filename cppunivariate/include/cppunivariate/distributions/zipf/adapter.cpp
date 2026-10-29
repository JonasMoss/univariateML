// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/zipf/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlzipf_(const Rcpp::IntegerVector &x, double reltol = 1.220703e-4,
                   int iterlim = 100) {
  if (x.size() == 0) {
    Rcpp::stop("x must have positive length");
  }
  if (x.size() > 0 &&
      std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    Rcpp::warning("all observations are zero");
  }

  auto res = cppunivariate::zipf_mle(dist_impl::as_ispan(x), reltol, iterlim);
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

#include "dpqr.h"
#include "cppunivariate/distributions/zipf/dpqr.hpp"
// [[Rcpp::export]]
Rcpp::NumericVector dzipf_(Rcpp::NumericVector x,Rcpp::NumericVector N,Rcpp::NumericVector s,bool log=false) {
  return dist_impl::dpqr_apply({x,N,s},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::dzipf(a[0],a[1],a[2],log);
  },true);
}
// [[Rcpp::export]]
Rcpp::NumericVector pzipf_(Rcpp::NumericVector q,Rcpp::NumericVector N,Rcpp::NumericVector s,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({q,N,s},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::pzipf(a[0],a[1],a[2],lower_tail,log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector qzipf_(Rcpp::NumericVector p,Rcpp::NumericVector N,Rcpp::NumericVector s,bool lower_tail=true,bool log_p=false) {
  return dist_impl::dpqr_apply({p,N,s},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::qzipf(a[0],a[1],a[2],lower_tail,log_p);
  });
}
// [[Rcpp::export]]
Rcpp::NumericVector rzipf_(int n,Rcpp::NumericVector N,Rcpp::NumericVector s) {
  return dist_impl::dpqr_apply({Rcpp::NumericVector(n),N,s},[&](const std::vector<double> &a) {
    return cppunivariate::distributions::qzipf(R::unif_rand(),a[1],a[2]);
  });
}
