// Lognormal — distribution metadata.
//
// See metadata/SCHEMA.md "Per-distribution form (C++ constexpr)".

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::lnorm {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "lnorm";
constexpr const char *name = "Lognormal";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"meanlog", "location", 0.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"sdlog", "scale", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::lnorm
