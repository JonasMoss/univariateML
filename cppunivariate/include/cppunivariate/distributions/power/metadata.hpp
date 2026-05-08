// PowerDist — distribution metadata.
//
// See metadata/SCHEMA.md "Per-distribution form (C++ constexpr)".

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::power {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "power";
constexpr const char *name = "PowerDist";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"alpha", "shape", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"beta", "shape", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::power
