#include "common/distributions/lomax/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mllomax_(const Rcpp::NumericVector &x,
                    Rcpp::Nullable<double> lambda0 = R_NilValue,
                    double reltol = 1.220703e-4, int iterlim = 100,
    Rcpp::Nullable<double> lambda = R_NilValue,
    Rcpp::Nullable<double> kappa = R_NilValue) {
  return common_result(umvl_binding::fit_lomax(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(lambda), common_held(kappa), common_held(lambda0), reltol, iterlim));
}
