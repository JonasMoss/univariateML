// Birnbaum-Saunders — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::fatigue {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "fatigue";
constexpr const char *name = "Birnbaum-Saunders";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"alpha", "shape", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"beta", "shape", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"mu", "location", 0.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::fatigue
