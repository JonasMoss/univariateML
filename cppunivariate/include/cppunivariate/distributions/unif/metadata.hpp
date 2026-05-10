// Uniform — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::unif {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "unif";
constexpr const char *name = "Uniform";

constexpr SupportDescriptor support{SupportType::Real, -kInf, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"min", "lower", 0.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"max", "upper", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::unif
