// Thin adapters from Rcpp::NumericVector / Rcpp::IntegerVector to
// mlcore::span / mlcore::ispan. Templated so this header has no R or
// Rcpp include dependency — instantiation happens at the call site
// (ml_exports.cpp), which already includes Rcpp.

#pragma once

#include "span.hpp"

#include <cstddef>

namespace mlcore {

template <typename NumVec>
inline span as_span(const NumVec &x) {
  return {x.begin(), static_cast<std::size_t>(x.size())};
}

template <typename IntVec>
inline ispan as_ispan(const IntVec &x) {
  return {x.begin(), static_cast<std::size_t>(x.size())};
}

}  // namespace mlcore
