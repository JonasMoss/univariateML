#pragma once
#include <string>
#include <vector>
#include <utility>
#include <cmath>
#include "cppunivariate/core/span.hpp"
namespace umvl_binding {
struct FitResult {
  std::vector<std::pair<std::string, double>> estimates;
  double logLik = NAN;
  int iterations = 0;
  std::vector<std::string> fixed;
  std::string error;
};
inline FitResult failure(const std::string& message) {
  FitResult result; result.error = message; return result;
}
struct Held {
  bool supplied = false;
  double value = NAN;
};
}
