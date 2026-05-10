// Loglogistic — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::llogis {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "llogis";
constexpr const char *name = "Loglogistic";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"shape", "shape", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"rate", "rate", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::llogis
