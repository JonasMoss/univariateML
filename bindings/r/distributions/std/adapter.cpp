#include "common/distributions/std/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlstd_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                  int iterlim = 200, Rcpp::Nullable<double> nu = R_NilValue, Rcpp::Nullable<double> mean = R_NilValue, Rcpp::Nullable<double> sd = R_NilValue) {
  const auto result = umvl_binding::fit_std(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mean), common_held(sd), common_held(nu), gradtol, iterlim);
  auto out = common_result(result);
  if (out.inherits("univariateML_stop")) {
    // Appending a named list element can replace the list and drop its class.
    const Rcpp::RObject classes = out.attr("class");
    for (const auto& item : result.extra) if (item.first == "certified") out["certified"] = item.second != 0;
    out.attr("class") = classes;
  } else {
    Rcpp::NumericVector estimates = out["estimates"];
    estimates.attr("certified") = Rcpp::as<double>(out["certified"]) != 0;
  }
  return out;
}
