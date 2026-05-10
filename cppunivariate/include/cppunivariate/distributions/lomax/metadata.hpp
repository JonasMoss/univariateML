// Lomax (Pareto Type II) — distribution metadata.
//
// Format constraints follow univariateML/metadata/SCHEMA.md, so the
// regex parser in univariateML/scripts/parse_cpp_metadata.R stays
// simple. Each ParamDescriptor entry is one logical line.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::lomax {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "lomax";
constexpr const char *name = "Lomax";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"lambda", "rate",  1.0, true, {SupportType::Real, 0.0, kInf, false, false}},
  {"kappa",  "shape", 2.0, true, {SupportType::Real, 0.0, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::lomax
