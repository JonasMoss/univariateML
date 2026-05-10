// Discrete uniform — distribution metadata.
//
// See metadata/SCHEMA.md "Per-distribution form (C++ constexpr)".

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::dunif {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "dunif";
constexpr const char *name = "Discrete uniform";

constexpr SupportDescriptor support{SupportType::Integer, -kInf, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"min", "lower", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"max", "upper", 10.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::dunif
