#pragma once

#include <cmath>
#include <limits>

namespace cppunivariate {
namespace special {

inline constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
inline constexpr double kPosInf = std::numeric_limits<double>::infinity();
inline constexpr double kNegInf = -std::numeric_limits<double>::infinity();

} // namespace special
} // namespace cppunivariate
