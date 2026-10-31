// Contract, retained choices, evidence and known numerical limitations:
// dev/research/evaluations/2026-10-31-step7-routines.md (repository root).
#pragma once
#include "span.hpp"
#include <cstddef>
namespace cppunivariate {
inline std::size_t collapse_multiplicity(span x) {
  double candidate = 0.0;
  std::size_t votes = 0;
  for (double v : x) {
    if (votes == 0) candidate = v;
    if (v == candidate) ++votes; else --votes;
  }
  std::size_t count = 0;
  for (double v : x) if (v == candidate) ++count;
  return count;
}
} // namespace cppunivariate
