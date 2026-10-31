#pragma once
#include <string>
#include <vector>
#include <utility>
#include <cmath>
#include <sstream>
#include <iomanip>
#include "cppunivariate/core/span.hpp"
namespace umvl_binding {
struct FitResult {
  std::vector<std::pair<std::string, double>> estimates;
  double logLik = NAN;
  int iterations = 0;
  std::vector<std::string> fixed;
  std::string error;
  std::vector<std::string> warnings;
  std::vector<std::pair<std::string, std::string>> restricted;
  std::vector<std::pair<std::string, double>> extra;
  std::vector<std::pair<std::string, std::vector<double>>> extra_vectors;
  bool has_iterations = false;
  bool has_fixed = false;
  bool has_restricted = false;
};
// The adapters use only %s, %g and fixed decimal %f diagnostics. Construct
// strings without host I/O, exceptions, varargs or an R formatting dependency.
template <typename T>
inline void message_argument(std::string& pattern, const T& value) {
  const std::size_t first = pattern.find('%');
  if (first == std::string::npos) return;
  std::size_t last = first + 1;
  int precision = 6;
  if (last < pattern.size() && pattern[last] == '.') {
    precision = 0;
    ++last;
    while (last < pattern.size() && pattern[last] >= '0' && pattern[last] <= '9') {
      precision = precision * 10 + pattern[last] - '0';
      ++last;
    }
  }
  if (last >= pattern.size()) return;
  std::ostringstream stream;
  stream << std::setprecision(precision);
  if (pattern[last] == 'f') stream << std::fixed;
  stream << value;
  pattern.replace(first, last - first + 1, stream.str());
}
template <typename... T>
inline std::string binding_message(std::string pattern, const T&... values) {
  (message_argument(pattern, values), ...);
  return pattern;
}
inline FitResult failure(const std::string& message, const std::vector<std::string>& warnings = {}) {
  FitResult result; result.error = message; result.warnings = warnings; return result;
}
struct Held {
  bool supplied = false;
  double value = NAN;
  std::size_t length = 1;
};
}
