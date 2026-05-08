// Loggamma — distribution metadata.
//
// See metadata/SCHEMA.md "Per-distribution form (C++ constexpr)".

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::lgamma {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "lgamma";
constexpr const char *name = "Loggamma";

constexpr SupportDescriptor support{SupportType::Real, 1.0, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"shapelog", "shape", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"ratelog", "rate", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::lgamma
