// Inverse Burr — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::invburr {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "invburr";
constexpr const char *name = "Inverse Burr";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"shape1", "shape", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"shape2", "shape", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::invburr
