// Nakagami — distribution metadata.
//
// See metadata/SCHEMA.md "Per-distribution form (C++ constexpr)".

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::naka {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "naka";
constexpr const char *name = "Nakagami";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"shape", "shape", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"scale", "scale", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::naka
