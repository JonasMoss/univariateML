// InvGamma — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::invgamma {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "invgamma";
constexpr const char *name = "InvGamma";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"alpha", "shape", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"beta",  "scale", 4.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::invgamma
