#pragma once
#include <algorithm>
#include <cstdint>
namespace cppunivariate {
// dev/design/work-budgets.md. Multiplication occurs after widening.
inline std::int64_t solver_budget(int maxit) {
  return static_cast<std::int64_t>(std::max(0, std::min(maxit, 100000)));
}
inline std::int64_t certificate_budget(int maxit) { return 128LL * solver_budget(maxit); }
inline std::int64_t enumeration_budget(int maxit) { return 100000LL * solver_budget(maxit); }
}
