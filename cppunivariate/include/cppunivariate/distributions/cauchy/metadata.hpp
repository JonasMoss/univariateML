// Cauchy — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::cauchy {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "cauchy";
constexpr const char *name = "Cauchy";

constexpr SupportDescriptor support{SupportType::Real, -kInf, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"location", "location", 0.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"scale", "scale", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::cauchy
