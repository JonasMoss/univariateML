// Fully held R models evaluate their density rather than optimize. Render that
// zero-dimensional estimate through the same outcome boundary as a core fit.
#include "common/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List ml_evaluated_result_(const Rcpp::NumericVector& coefficients, double loglik) {
  umvl_binding::FitResult result;
  const Rcpp::CharacterVector names=coefficients.names();
  for(R_xlen_t i=0;i<coefficients.size();++i)
    result.estimates.push_back({Rcpp::as<std::string>(names[i]),coefficients[i]});
  result.logLik=loglik;
  return common_result(umvl_binding::with_outcome(std::move(result),cppunivariate::FitOutcome::singleton(true),-1));
}
