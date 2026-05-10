// Geometric — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::geom {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "geom";
constexpr const char *name = "Geometric";

constexpr SupportDescriptor support{SupportType::Integer, 0.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"prob", "probability", 0.5, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::geom
