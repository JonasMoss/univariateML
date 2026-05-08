// Burr — distribution metadata.
//
// See metadata/SCHEMA.md "Per-distribution form (C++ constexpr)".

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::burr {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "burr";
constexpr const char *name = "Burr";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"shape1", "shape", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"shape2", "shape", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::burr
