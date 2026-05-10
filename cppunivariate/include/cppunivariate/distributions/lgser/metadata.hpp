// Logarithmic series — distribution metadata.
//
// See metadata/SCHEMA.md "Per-distribution form (C++ constexpr)".

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::lgser {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "lgser";
constexpr const char *name = "Logarithmic series";

constexpr SupportDescriptor support{SupportType::Integer, 1.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"theta", "rate", 0.9, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::lgser
