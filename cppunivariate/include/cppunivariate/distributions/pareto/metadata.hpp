// Pareto — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::pareto {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "pareto";
constexpr const char *name = "Pareto";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"a", "shape", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"b", "shape", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::pareto
