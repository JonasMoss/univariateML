#include "common/distributions/gpd/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlgpd_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mu,
                  Rcpp::Nullable<double> xi = R_NilValue,
                  Rcpp::Nullable<double> sigma = R_NilValue,
                  double reltol = 1e-12, int iterlim = 200) {
  return common_result(umvl_binding::fit_gpd(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma), common_held(xi), reltol, iterlim));
}
