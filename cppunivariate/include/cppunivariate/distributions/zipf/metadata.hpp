// Zipf — distribution metadata.
//
// See metadata/SCHEMA.md "Per-distribution form (C++ constexpr)".

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::zipf {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "zipf";
constexpr const char *name = "Zipf";

constexpr SupportDescriptor support{SupportType::Integer, 1.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"N", "trials", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"s", "shape", 0.5, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::zipf
