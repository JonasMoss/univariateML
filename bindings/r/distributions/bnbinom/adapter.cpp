#include "common/distributions/bnbinom/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlbnbinom_(const Rcpp::IntegerVector &x,
    Rcpp::Nullable<double> size=R_NilValue,
    Rcpp::Nullable<double> alpha=R_NilValue,
    Rcpp::Nullable<double> beta=R_NilValue,
    double gradtol=1e-10, int iterlim=600) {
  const auto fit=umvl_binding::fit_bnbinom(cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, common_held(size), common_held(alpha), common_held(beta), gradtol, iterlim);
  auto out=common_result(fit);
  const Rcpp::RObject classes=out.attr("class");
  for(const auto &item:fit.extra)if(item.first=="certified") {
    const bool certified=item.second!=0;
    out["certified"]=certified;
    if(fit.error.empty()) {
      Rcpp::NumericVector estimates=out["estimates"];
      estimates.attr("certified")=certified;
      out["estimates"]=estimates;
    }
  }
  if(!classes.isNULL())out.attr("class")=classes;
  return out;
}
