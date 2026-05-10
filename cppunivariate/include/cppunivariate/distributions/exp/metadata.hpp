// Exponential — distribution metadata.
//
// Format constraints follow univariateML/metadata/SCHEMA.md.
// One-parameter exponential family with rate parameter; closed-form MLE.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::exp {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "exp";
constexpr const char *name = "Exponential";

constexpr SupportDescriptor support{SupportType::Real, 0.0, kInf, true, false};

constexpr ParamDescriptor params[] = {
  {"rate", "rate", 1.0, true, {SupportType::Real, 0.0, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::exp
