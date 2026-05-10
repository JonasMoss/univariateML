// Gumbel — distribution metadata.
//
// See metadata/SCHEMA.md "Per-distribution form (C++ constexpr)".

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::gumbel {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "gumbel";
constexpr const char *name = "Gumbel";

constexpr SupportDescriptor support{SupportType::Real, -kInf, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"mu", "location", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"sigma", "scale", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::gumbel
