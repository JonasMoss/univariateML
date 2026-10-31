#include "common/distributions/paralogis/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlparalogis_(const Rcpp::NumericVector &x,
                        Rcpp::Nullable<double> shape0 = R_NilValue,
                        double gradtol = 1.220703e-4, int iterlim = 100,
                        Rcpp::Nullable<double> scale = R_NilValue,
                        Rcpp::Nullable<double> shape = R_NilValue) {
  return common_result(umvl_binding::fit_paralogis(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape), common_held(scale), common_held(shape0), gradtol, iterlim));
}
