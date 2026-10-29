#include <Rcpp.h>
#include <cmath>
// [[Rcpp::export]]
Rcpp::NumericVector cumlse(Rcpp::NumericVector v) {
  Rcpp::NumericVector o(v.size()); double acc = -INFINITY;
  for (R_xlen_t i = 0; i < v.size(); ++i) {
    double b = v[i];
    if (std::isfinite(b) || b > 0) {
      if (!std::isfinite(acc)) acc = b;
      else { double m = acc > b ? acc : b; acc = m + std::log1p(std::exp(-std::fabs(acc - b))); }
    }
    o[i] = acc;
  }
  return o;
}
