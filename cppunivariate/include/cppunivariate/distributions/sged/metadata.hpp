// Skew Generalized Error — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::sged {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "sged";
constexpr const char *name = "Skew Generalized Error";

constexpr SupportDescriptor support{SupportType::Real, -kInf, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"mean", "location", 0.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"sd", "scale", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"nu", "df", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"xi", "skew", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::sged
