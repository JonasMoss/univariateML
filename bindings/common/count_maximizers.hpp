#pragma once
#include "fit.hpp"
#include <initializer_list>

namespace umvl_binding {
inline cppunivariate::Axis count_sizes(double minimum, Held size) {
  if (size.supplied && size.value > cppunivariate::maxset_count) {
    // These binomial routes also accept exactly represented held sizes above
    // the consecutive-integer range. The closed singleton needs no lattice.
    return cppunivariate::fixed(size.value);
  }
  return cppunivariate::interval(minimum, cppunivariate::maxset_inf, true, false, true);
}

inline void attach_count_set(FitResult& out, cppunivariate::FitOutcome& outcome,
    cppunivariate::MaximizerSet set, const std::string& model,
    std::initializer_list<Held> held) {
  if (outcome.kind != cppunivariate::OutcomeKind::Estimate || out.estimates.empty())
    return;
  std::array<std::optional<double>, cppunivariate::maxset_dim> slice{};
  std::size_t coordinate = 0;
  for (const auto& hold : held) {
    if (hold.supplied) slice[coordinate] = hold.value;
    ++coordinate;
  }
  if (set.slice(slice) != cppunivariate::MaxsetStatus::Ok) return;
  outcome.unique = set.singleton();
  outcome.uniqueness_known = true;
  if (!outcome.unique)
    attach_maximizer_set(out, set, model, [&](const auto&) { return out.logLik; });
}
} // namespace umvl_binding
