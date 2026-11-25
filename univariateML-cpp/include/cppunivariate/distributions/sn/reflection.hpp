#pragma once

#include "../../core/maximizer_set.hpp"
#include <algorithm>
#include <vector>

namespace cppunivariate {
namespace skew_reflection_detail {

// The two words represent twice the reflection center, or the center itself
// if the endpoint sum overflows. No representable midpoint is required.
struct Center {
  double high = NAN, low = 0;
  bool halved = false;

  double reflect(double location) const {
    return halved ? std::fma(2., high, -location) + 2*low
                  : (high-location) + low;
  }
};

// FastTwoSum under round-to-nearest with gradual underflow.
inline std::array<double, 2> exact_sum(double a, double b, bool halve) {
  if (halve) {
    // Only sums of overflowing magnitude can match a halved endpoint sum.
    // The inputs of such a sum are large enough for exact halving.
    if (std::isfinite(a+b)) return {NAN, NAN};
    a *= .5;
    b *= .5;
  }
  if (std::abs(a) < std::abs(b)) std::swap(a,b);
  const double high = a+b;
  return {high, b-(high-a)};
}

inline Center symmetric_center(span sorted, double held_location) {
  if (sorted.empty()) return {};
  const bool halve = !std::isfinite(sorted[0]+sorted[sorted.size()-1]);
  const auto sum = exact_sum(sorted[0],sorted[sorted.size()-1],halve);
  if (!std::isnan(held_location) && exact_sum(held_location,held_location,halve) != sum)
    return {};
  for (std::size_t i=1; i<sorted.size()-i; ++i)
    if (exact_sum(sorted[i],sorted[sorted.size()-1-i],halve) != sum) return {};
  return {sum[0],sum[1],halve};
}

inline Center symmetric_center_unsorted(span x, double held_location) {
  std::vector<double> sorted(x.begin(),x.end());
  std::sort(sorted.begin(),sorted.end());
  return symmetric_center({sorted.data(),sorted.size()},held_location);
}

inline MaximizerSet reflected_pair(double location, double scale, double shape,
                                   double other_location, double other_shape) {
  MaximizerSet set;
  const std::array<Point,2> points{Point{{{location,scale,shape}},{}},
                                  Point{{{other_location,scale,other_shape}},{}}};
  set.dimension = 3;
  if (set.add_points({points.data(),points.size()}) != MaxsetStatus::Ok) return {};
  return set;
}

}  // namespace skew_reflection_detail
}  // namespace cppunivariate
