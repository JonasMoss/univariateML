#include "common/distributions/pois/tpois_fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List modifier_tpois_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> a, Rcpp::Nullable<double> b, Rcpp::Nullable<double> lambda=R_NilValue, double tol = 1e-10, int iterlim = 200) {
  auto out = umvl_binding::fit_tpois(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(lambda), common_held(a), common_held(b), tol, iterlim);
  if (out.estimates.size() == 3)
    umvl_binding::attach_tpois_window_set(out, std::floor(out.estimates[1].second)+1);
  return common_result(out);
}
