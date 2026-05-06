// Bijections between unconstrained `phi` (what BFGS sees) and constrained
// `theta` (what the density wants). We optimise the negative log-likelihood
// of the transformed parameters directly: the Jacobian terms are additive
// constants for MLE and don't shift the argmax.
//
// `_shift` variants handle bounds with a known floor c (e.g. nu > 2 for
// Student-t / sstd). No logit — none of the six fGarch-family estimators
// need a bounded-interval reparam.

#pragma once

#include <cmath>

namespace mlcore {

inline double from_log(double phi) { return std::exp(phi); }
inline double to_log(double x) { return std::log(x); }

inline double from_log_shift(double phi, double c) {
  return std::exp(phi) + c;
}
inline double to_log_shift(double x, double c) { return std::log(x - c); }

}  // namespace mlcore
