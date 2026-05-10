// Shared types for per-distribution `metadata.hpp` files in
// distributions/<id>/metadata.hpp. Each distribution's metadata.hpp
// declares constexpr instances of these to expose its identity,
// parameters, and support to C++ consumers at compile time.
//
// The R-side codegen also reads metadata.hpp via a regex parser
// (univariateML/scripts/parse_cpp_metadata.R), which keys on a constrained
// layout: see univariateML/metadata/SCHEMA.md.

#pragma once

#include <cstddef>

namespace cppunivariate {

enum class SupportType { Real, Integer };

struct SupportDescriptor {
  SupportType type;
  double lower;
  double upper;
  bool lower_closed;
  bool upper_closed;
};

struct ParamDescriptor {
  const char *name;
  const char *role;       // free-form: "rate", "shape", "location", "scale", ...
  double default_value;
  bool estimable;         // forward-looking; today always true (see roadmap)
  SupportDescriptor support;
};

}  // namespace cppunivariate
