// Shared R-shell recycling, missing-value propagation and warning policy.
#pragma once
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>
namespace dist_impl {
template <typename Evaluate>
Rcpp::NumericVector dpqr_apply(const std::vector<Rcpp::NumericVector> &args,
                               Evaluate evaluate, bool density=false) {
  R_xlen_t n=0;
  for (const auto &v:args) { if (!v.size()) return Rcpp::NumericVector(0); n=std::max(n,v.size()); }
  Rcpp::NumericVector out(n);
  std::vector<double> row(args.size());
  bool invalid=false, fractional=false;
  for (R_xlen_t i=0;i<n;++i) {
    bool na=false,nan=false;
    for (std::size_t j=0;j<args.size();++j) {
      row[j]=args[j][i%args[j].size()];
      na=na || R_IsNA(row[j]); nan=nan || R_IsNaN(row[j]);
    }
    if (na || nan) { out[i]=na ? NA_REAL : R_NaN; continue; }
    out[i]=evaluate(row);
    invalid=invalid || std::isnan(out[i]);
    fractional=fractional || (density && std::isfinite(row[0]) && row[0]!=std::floor(row[0]));
  }
  if (invalid) Rcpp::warning("NaNs produced");
  if (fractional) Rcpp::warning("non-integer x");
  return out;
}
}
