#include "common/distributions/sgomp/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlsgomp_(const Rcpp::NumericVector &x,
                    Rcpp::Nullable<double> b=R_NilValue,
                    Rcpp::Nullable<double> eta=R_NilValue,
                    double reltol=1e-10,int iterlim=200) {
  return common_result(umvl_binding::fit_sgomp(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(b), common_held(eta), reltol, iterlim));
}
