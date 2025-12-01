#pragma once

#include <Rmath.h>
#include <cmath>
#include <limits>
#include <string>
#include <variant>

inline constexpr double kEps = std::numeric_limits<double>::epsilon();
inline constexpr double kSqrtEps = std::sqrt(kEps);
inline constexpr double kTolDefault = std::sqrt(kSqrtEps); // epsilon^(1/4)
inline constexpr int kMaxIterDefault = 100;
