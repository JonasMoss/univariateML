// C++17 layout and exact integer ratio smoke test for the design.
#include <array>
#include <cassert>
#include <cstdint>
#include <numeric>
#include <optional>
#include <type_traits>
#include <variant>

namespace cppunivariate {
constexpr std::size_t maxset_dim = 5;

struct Interval {
  double lower, upper;
  bool lower_closed, upper_closed;
};

struct Axis {
  Interval range;  // Equal closed ends encode a fixed coordinate.
  bool integer;
};

struct Coordinates {
  std::array<double, maxset_dim> values;
};

struct Enclosure {
  Coordinates lower, upper;
};

struct Point {
  Coordinates estimate;
  std::optional<Enclosure> enclosure;
};

struct Points {
  std::array<Point, 4> values;
  std::uint8_t size;
};

struct CountOrder {
  std::uint8_t lhs, rhs;  // x[lhs] <= x[rhs]
};

struct Box {
  std::array<Axis, maxset_dim> axes;
  std::array<CountOrder, 4> order;
  std::uint8_t order_size;
};

enum class RelationForm { Product, PartOverSum, CountRatio, Semicircle };

struct Level {
  double value;
  std::uint64_t numerator, denominator; // denominator == 0 selects value.
};

struct Relation {
  std::array<Axis, maxset_dim> axes;
  RelationForm form;
  Level level;
  std::array<double, 2> anchors;  // Semicircle only.
  std::uint8_t driver, dependent;
  bool complement_driver, complement_dependent;  // Product operands only.
};

struct Piece {
  std::variant<Points, Box, Relation> value;
};

struct MaximizerSet {
  std::array<Piece, 3> pieces;
  std::uint8_t dimension, size;
};

inline bool count_ratio(std::uint64_t a, std::uint64_t b, Level level) noexcept {
  if (b == 0 || level.denominator == 0) return false;
  const auto divisor = std::gcd(a, b);
  return a / divisor == level.numerator && b / divisor == level.denominator;
}
}  // namespace cppunivariate

int main() {
  static_assert(std::is_trivially_copyable<cppunivariate::MaximizerSet>::value);
  static_assert(sizeof(cppunivariate::MaximizerSet) <= 2048);
  constexpr std::uint64_t bound = (std::uint64_t{1} << 53) - 1;
  const cppunivariate::Level half{0, 1, 2};
  assert(cppunivariate::count_ratio(1, 2, half));
  assert(cppunivariate::count_ratio(bound / 2, bound - 1, half));
  assert(!cppunivariate::count_ratio(bound / 2, bound, half));
  assert(!cppunivariate::count_ratio(0, 0, half));
}
