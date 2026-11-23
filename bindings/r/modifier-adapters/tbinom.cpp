#include "common/distributions/binom/tbinom_fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List modifier_tbinom_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> a, Rcpp::Nullable<double> b, Rcpp::Nullable<double> size, Rcpp::Nullable<double> prob=R_NilValue) {
  auto out = umvl_binding::fit_tbinom_controlled(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(prob), common_held(a), common_held(b), common_held(size),1e-10,512,true);
  if (out.estimates.size() == 4)
    umvl_binding::attach_tbinom_window_set(out, std::max(0., std::floor(out.estimates[1].second)+1),
        std::floor(out.estimates[2].second), common_held(size), common_held(prob));
  return common_result(out);
}
