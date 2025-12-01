#pragma once

#include <cmath>
#include <limits>

inline constexpr double kEps      = std::numeric_limits<double>::epsilon();
inline constexpr double kSqrtEps  = std::sqrt(kEps);
