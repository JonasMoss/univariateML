// Rayleigh — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::rayleigh {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "rayleigh";
constexpr const char *name = "Rayleigh";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"sigma", "scale", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::rayleigh
