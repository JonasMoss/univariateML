// Zero-inflated Poisson — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::zip {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "zip";
constexpr const char *name = "Zero-inflated Poisson";

constexpr SupportDescriptor support{SupportType::Integer, 0.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"lambda", "rate", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"pi", "probability", 0.5, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::zip
