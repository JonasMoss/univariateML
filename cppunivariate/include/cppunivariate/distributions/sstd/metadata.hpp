// Skew Student-t — distribution metadata.
//
// See univariateML/metadata/SCHEMA.md.

#pragma once

#include "../../core/metadata.hpp"

#include <cstddef>
#include <iterator>
#include <limits>

namespace cppunivariate::distributions::sstd {

constexpr double kInf = std::numeric_limits<double>::infinity();

constexpr const char *id   = "sstd";
constexpr const char *name = "Skew Student-t";

constexpr SupportDescriptor support{SupportType::Real, -kInf, kInf, false, false};

constexpr ParamDescriptor params[] = {
  {"mean", "location", 0.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"sd", "scale", 1.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"nu", "df", 3.0, true, {SupportType::Real, -kInf, kInf, false, false}},
  {"xi", "skew", 2.0, true, {SupportType::Real, -kInf, kInf, false, false}},
};

constexpr std::size_t n_params = std::size(params);

}  // namespace cppunivariate::distributions::sstd
