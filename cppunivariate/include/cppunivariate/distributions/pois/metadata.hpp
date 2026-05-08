// Poisson — distribution metadata.
//
// See metadata/SCHEMA.md "Per-distribution form (C++ constexpr)".

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::pois {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "pois";
constexpr const char *name = "Poisson";

constexpr SupportDescriptor support{SupportType::Integer, 0.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"lambda", "rate", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::pois
