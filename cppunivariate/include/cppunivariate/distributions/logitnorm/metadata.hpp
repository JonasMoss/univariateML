// LogitNormal — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::logitnorm {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "logitnorm";
constexpr const char *name = "LogitNormal";

constexpr SupportDescriptor support{SupportType::Real, 0.0, 1.0, false, false};

constexpr ParamDescriptor params[] = {
  {"mu", "location", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"sigma", "scale", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::logitnorm
