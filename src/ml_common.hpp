#pragma once

#include <cmath>
#include <limits>

inline constexpr double kEps      = std::numeric_limits<double>::epsilon();
inline constexpr double kSqrtEps  = std::sqrt(kEps);
inline constexpr double kTolDefault = std::sqrt(kSqrtEps); // epsilon^(1/4)
inline constexpr int    kMaxIterDefault = 100;
