// Paralogistic — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::paralogis {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "paralogis";
constexpr const char *name = "Paralogistic";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"shape", "shape", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"scale", "scale", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::paralogis
