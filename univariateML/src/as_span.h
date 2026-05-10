// Thin adapters from Rcpp::NumericVector / Rcpp::IntegerVector to
// cppunivariate::span / cppunivariate::ispan, used by the Rcpp wrappers
// in univariateML/src/ml_exports.cpp. Templated so this header has no Rcpp include
// dependency -- instantiation happens at the call site, which already
// includes Rcpp.

#pragma once

#include "cppunivariate/core/span.hpp"

#include <cstddef>

namespace dist_impl {

template <typename NumVec>
inline cppunivariate::span as_span(const NumVec &x) {
  return {x.begin(), static_cast<std::size_t>(x.size())};
}

template <typename IntVec>
inline cppunivariate::ispan as_ispan(const IntVec &x) {
  return {x.begin(), static_cast<std::size_t>(x.size())};
}

}  // namespace dist_impl
