// Laplace — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::laplace {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "laplace";
constexpr const char *name = "Laplace";

constexpr SupportDescriptor support{SupportType::Real, -kInf, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"mu", "location", 0.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"sigma", "scale", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::laplace
