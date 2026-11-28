#pragma once

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <limits>
#include <numeric>
#include <optional>
#include <tuple>
#include <variant>

#include "span.hpp"

namespace cppunivariate {

inline constexpr std::size_t maxset_dim = 5;
inline constexpr double maxset_count = 9007199254740991.0;
inline constexpr double maxset_inf = std::numeric_limits<double>::infinity();

struct Interval {
  double lower = -maxset_inf, upper = maxset_inf;
  bool lower_closed = false, upper_closed = false;
  bool lower_rounded = false, upper_rounded = false;
};
struct Axis { Interval range; bool integer = false; };
inline Axis interval(double lower, double upper, bool lower_closed, bool upper_closed,
                     bool integer = false) {
  return {{lower, upper, lower_closed, upper_closed}, integer};
}
inline Axis fixed(double value, bool integer = false) {
  return interval(value, value, true, true, integer);
}
struct Coordinates { std::array<double, maxset_dim> values{}; };
struct Enclosure { Coordinates lower, upper; };
struct Point { Coordinates estimate; std::optional<Enclosure> enclosure; };
struct Points { std::array<Point, 4> values{}; std::uint8_t size = 0; };
struct CountOrder { std::uint8_t lhs = 0, rhs = 0; };
struct Box {
  std::array<Axis, maxset_dim> axes{};
  std::array<CountOrder, 4> order{};
  std::uint8_t order_size = 0;
};
enum class RelationForm { Product, PartOverSum, CountRatio, Semicircle, AdjacentBinomial };
struct Level {
  double value = 0;
  std::uint64_t numerator = 0, denominator = 0;
  bool rounded = false;
};
struct Relation {
  std::array<Axis, maxset_dim> axes{};
  RelationForm form = RelationForm::Product;
  Level level{};
  std::array<double, 2> anchors{};
  std::uint8_t driver = 0, dependent = 1;
  bool complement_driver = false, complement_dependent = false;
};
struct Piece { std::variant<Points, Box, Relation> value; };
enum class MaxsetStatus { Ok, Empty, Invalid, Capacity };

inline double maxset_zero(double value) { return value == 0 ? 0 : value; }

// The largest active array has four entries. Insertion sort stays inside
// that fixed capacity without instantiating a general sort's pivot machinery.
template <typename T, std::size_t N, typename Compare>
inline void maxset_sort(std::array<T, N>& values, std::size_t size, Compare before) {
  for (std::size_t i = 1; i < size && i < N; ++i)
    for (std::size_t j = i; j && before(values[j], values[j - 1]); --j)
      std::swap(values[j], values[j - 1]);
}

inline bool maxset_overlap(const Point& first, const Point& second, std::size_t dimension) {
  for (std::size_t i = 0; i < dimension; ++i) {
    const double a = first.enclosure ? first.enclosure->lower.values[i] : first.estimate.values[i];
    const double b = first.enclosure ? first.enclosure->upper.values[i] : first.estimate.values[i];
    const double c = second.enclosure ? second.enclosure->lower.values[i] : second.estimate.values[i];
    const double d = second.enclosure ? second.enclosure->upper.values[i] : second.estimate.values[i];
    if (a > d || c > b) return false;
  }
  return true;
}

inline bool contains(const Axis& axis, double value) {
  const auto& r = axis.range;
  return std::isfinite(value) &&
      (!axis.integer || std::trunc(value) == value) &&
      (value > r.lower || (r.lower_closed && value == r.lower)) &&
      (value < r.upper || (r.upper_closed && value == r.upper));
}

inline MaxsetStatus normalize(Axis& axis) {
  auto& r = axis.range;
  if (std::isnan(r.lower) || std::isnan(r.upper) || r.lower == maxset_inf ||
      r.upper == -maxset_inf || r.lower > r.upper || (!std::isfinite(r.lower) && r.lower_closed) ||
      (!std::isfinite(r.upper) && r.upper_closed))
    return MaxsetStatus::Invalid;
  if (axis.integer) {
    if (r.lower_rounded || r.upper_rounded ||
        (std::isfinite(r.lower) && std::abs(r.lower) > maxset_count) ||
        (std::isfinite(r.upper) && std::abs(r.upper) > maxset_count))
      return MaxsetStatus::Invalid;
    if (std::isfinite(r.lower)) {
      r.lower = r.lower_closed ? std::ceil(r.lower) : std::floor(r.lower) + 1;
      r.lower_closed = true;
    }
    if (std::isfinite(r.upper)) {
      r.upper = r.upper_closed ? std::floor(r.upper) : std::ceil(r.upper) - 1;
      r.upper_closed = true;
    }
    if (r.lower > r.upper) return MaxsetStatus::Empty;
  }
  r.lower = maxset_zero(r.lower);
  r.upper = maxset_zero(r.upper);
  if (r.lower > r.upper) return MaxsetStatus::Invalid;
  if (r.lower == r.upper && !(r.lower_closed && r.upper_closed)) return MaxsetStatus::Empty;
  return MaxsetStatus::Ok;
}

inline MaxsetStatus normalize(Box& box, std::size_t dimension) {
  if (box.order_size > box.order.size()) return MaxsetStatus::Invalid;
  for (std::size_t i = 0; i < dimension; ++i) {
    const auto status = normalize(box.axes[i]);
    if (status != MaxsetStatus::Ok) return status;
  }
  auto end = box.order.begin() + box.order_size;
  for (auto it = box.order.begin(); it != end; ++it)
    if (it->lhs >= dimension || it->rhs >= dimension ||
        !box.axes[it->lhs].integer || !box.axes[it->rhs].integer)
      return MaxsetStatus::Invalid;
  maxset_sort(box.order, box.order_size, [](CountOrder a, CountOrder b) {
    return a.lhs != b.lhs ? a.lhs < b.lhs : a.rhs < b.rhs;
  });
  end = std::remove_if(box.order.begin(), end, [](CountOrder o) { return o.lhs == o.rhs; });
  end = std::unique(box.order.begin(), end, [](CountOrder a, CountOrder b) {
    return a.lhs == b.lhs && a.rhs == b.rhs;
  });
  box.order_size = static_cast<std::uint8_t>(end - box.order.begin());
  for (std::size_t pass = 0; pass < dimension; ++pass) {
    for (auto it = box.order.begin(); it != end; ++it) {
      auto& a = box.axes[it->lhs].range;
      auto& b = box.axes[it->rhs].range;
      a.upper = std::min(a.upper, b.upper);
      b.lower = std::max(b.lower, a.lower);
      a.upper_closed = std::isfinite(a.upper);
      b.lower_closed = std::isfinite(b.lower);
    }
  }
  for (std::size_t i = 0; i < dimension; ++i)
    if (box.axes[i].range.lower > box.axes[i].range.upper) return MaxsetStatus::Empty;
  end = std::remove_if(box.order.begin(), end, [&](CountOrder o) {
    return box.axes[o.lhs].range.upper <= box.axes[o.rhs].range.lower;
  });
  box.order_size = static_cast<std::uint8_t>(end - box.order.begin());
  return MaxsetStatus::Ok;
}

inline bool contains(const Box& box, Coordinates x, std::size_t dimension) {
  if (dimension > maxset_dim || box.order_size > box.order.size()) return false;
  for (std::size_t i = 0; i < dimension; ++i)
    if (!contains(box.axes[i], x.values[i])) return false;
  for (std::size_t i = 0; i < box.order_size; ++i) {
    const auto order = box.order[i];
    if (order.lhs >= dimension || order.rhs >= dimension ||
        x.values[order.lhs] > x.values[order.rhs]) return false;
  }
  return true;
}

inline double relation_level(const Relation& r) {
  return r.level.denominator ? static_cast<double>(r.level.numerator) /
      static_cast<double>(r.level.denominator) : r.level.value;
}

inline double relation_solve(const Relation& r, double x) {
  if (r.form == RelationForm::Semicircle) {
    const double a = r.anchors[0], b = r.anchors[1];
    if (!(a < x && x < b)) return std::numeric_limits<double>::quiet_NaN();
    // Multiply significands and halve the exponent before taking the
    // square root. This avoids overflow, underflow and two rounded roots.
    double left = x - a, right = b - x;
    int extra_exponent = 0;
    if (!std::isfinite(left)) {
      left = x * .5 - a * .5;
      ++extra_exponent;
    }
    if (!std::isfinite(right)) {
      right = b * .5 - x * .5;
      ++extra_exponent;
    }
    int left_exponent, right_exponent;
    const double l = std::frexp(left, &left_exponent);
    const double r = std::frexp(right, &right_exponent);
    int exponent = left_exponent + right_exponent + extra_exponent;
    double product = l * r;
    if (exponent % 2 != 0) {
      product *= 2;
      --exponent;
    }
    return std::ldexp(std::sqrt(product), exponent / 2);
  }
  const double level = relation_level(r);
  if (r.form == RelationForm::PartOverSum) {
    if (r.level.denominator)
      return x * (static_cast<double>(r.level.denominator - r.level.numerator) /
                  static_cast<double>(r.level.numerator));
    return x * ((1 - level) / level);
  }
  if (r.form == RelationForm::AdjacentBinomial)
    return level / ((x - r.anchors[0]) + level);
  if (r.form == RelationForm::CountRatio) {
    if (x < 0 || x > maxset_count || !std::isfinite(x) || std::trunc(x) != x ||
        !r.level.numerator || !r.level.denominator) return std::numeric_limits<double>::quiet_NaN();
    const auto count = static_cast<std::uint64_t>(x);
    const auto multiplier = count / r.level.numerator;
    if (count % r.level.numerator ||
        multiplier > static_cast<std::uint64_t>(maxset_count) / r.level.denominator)
      return std::numeric_limits<double>::quiet_NaN();
    return static_cast<double>(multiplier * r.level.denominator);
  }
  const double factor = r.complement_driver ? 1 - x : x;
  const double dependent = level / factor;
  return r.complement_dependent ? 1 - dependent : dependent;
}

inline bool relation_intersects(const Relation& relation) {
  const auto& x = relation.axes[relation.driver].range;
  const auto& y = relation.axes[relation.dependent].range;
  const auto evaluate = [&](double value) {
    if (relation.form == RelationForm::Semicircle &&
        (value == relation.anchors[0] || value == relation.anchors[1])) return 0.;
    return relation_solve(relation, value);
  };
  double lo = evaluate(x.lower), hi = evaluate(x.upper);
  bool lc = x.lower_closed, hc = x.upper_closed;
  if (lo > hi) {
    std::swap(lo, hi);
    std::swap(lc, hc);
  }
  if (relation.form == RelationForm::Semicircle) {
    const double midpoint = relation.anchors[0] * .5 + relation.anchors[1] * .5;
    if (x.lower < midpoint && midpoint < x.upper) {
      hi = relation.anchors[1] * .5 - relation.anchors[0] * .5;
      hc = true;
    }
  }
  if (lo == hi && x.lower != x.upper) {
    // Rounded dependent values do not prove a singleton image.
    return !(hi < y.lower || lo > y.upper);
  }
  return (y.lower < hi || (y.lower == hi && y.lower_closed && hc)) &&
         (lo < y.upper || (lo == y.upper && lc && y.upper_closed));
}

inline MaxsetStatus normalize(Relation& r, std::size_t dimension) {
  if (r.driver >= dimension || r.dependent >= dimension || r.driver == r.dependent ||
      (r.form != RelationForm::Product && (r.complement_driver || r.complement_dependent)))
    return MaxsetStatus::Invalid;
  for (std::size_t i = 0; i < dimension; ++i) {
    const auto status = normalize(r.axes[i]);
    if (status != MaxsetStatus::Ok) return status;
  }
  for (std::size_t i = 0; i < dimension; ++i)
    if (i != r.driver && i != r.dependent &&
        r.axes[i].range.lower != r.axes[i].range.upper) return MaxsetStatus::Invalid;
  const auto& x = r.axes[r.driver];
  const auto& y = r.axes[r.dependent];
  if (r.form == RelationForm::Semicircle) {
    if (x.integer || y.integer || !std::isfinite(r.anchors[0]) ||
        !std::isfinite(r.anchors[1]) || !(r.anchors[0] < r.anchors[1]) ||
        x.range.lower < r.anchors[0] || x.range.upper > r.anchors[1] ||
        (x.range.lower == r.anchors[0] && x.range.lower_closed) ||
        (x.range.upper == r.anchors[1] && x.range.upper_closed) ||
        y.range.lower < 0 || (y.range.lower == 0 && y.range.lower_closed))
      return MaxsetStatus::Invalid;
    r.anchors[0] = maxset_zero(r.anchors[0]);
    r.anchors[1] = maxset_zero(r.anchors[1]);
    r.level = {};
    return relation_intersects(r) ? MaxsetStatus::Ok : MaxsetStatus::Empty;
  }
  if (r.form != RelationForm::AdjacentBinomial) r.anchors = {};
  if (r.level.denominator) {
    if (r.level.rounded) return MaxsetStatus::Invalid;
    const auto divisor = std::gcd(r.level.numerator, r.level.denominator);
    r.level.numerator /= divisor;
    r.level.denominator /= divisor;
    r.level.value = 0;
  } else r.level.numerator = 0;
  switch (r.form) {
    case RelationForm::CountRatio:
      if (!x.integer || !y.integer || !r.level.numerator || !r.level.denominator ||
          r.level.rounded || r.level.numerator > r.level.denominator ||
          r.level.denominator > static_cast<std::uint64_t>(maxset_count) ||
          x.range.lower <= 0 || y.range.lower <= 0) return MaxsetStatus::Invalid;
      {
        const auto first = [](double lo, std::uint64_t n) {
          const auto v = static_cast<std::uint64_t>(lo);
          return v / n + (v % n != 0);
        };
        const auto last = [](double hi, std::uint64_t n) {
          return static_cast<std::uint64_t>(std::min(hi, maxset_count)) / n;
        };
        const auto n = r.level.numerator, d = r.level.denominator;
        const auto lo = std::max(first(x.range.lower, n), first(y.range.lower, d));
        const auto hi = std::min(last(x.range.upper, n), last(y.range.upper, d));
        if (lo > hi) return MaxsetStatus::Empty;
        r.axes[r.driver] = interval(static_cast<double>(lo * n), static_cast<double>(hi * n), true, true, true);
        r.axes[r.dependent] = interval(static_cast<double>(lo * d), static_cast<double>(hi * d), true, true, true);
      }
      break;
    case RelationForm::AdjacentBinomial:
      if (!x.integer || y.integer || !std::isfinite(r.anchors[0]) ||
          r.anchors[0] < 0 || std::floor(r.anchors[0]) != r.anchors[0] ||
          x.range.lower <= r.anchors[0] || y.range.lower < 0 || y.range.upper > 1 ||
          (y.range.lower == 0 && y.range.lower_closed) ||
          (y.range.upper == 1 && y.range.upper_closed) ||
          !r.level.numerator || !r.level.denominator)
        return MaxsetStatus::Invalid;
      r.anchors[1] = 0;
      break;
    case RelationForm::PartOverSum:
      if (x.integer || y.integer || x.range.lower < 0 || y.range.lower < 0 ||
          (x.range.lower == 0 && x.range.lower_closed) ||
          (y.range.lower == 0 && y.range.lower_closed) ||
          !(relation_level(r) > 0 && relation_level(r) < 1)) return MaxsetStatus::Invalid;
      break;
    case RelationForm::Product:
      if (x.integer || y.integer || !std::isfinite(relation_level(r)) ||
          !(relation_level(r) > 0) ||
          (r.complement_driver ? x.range.upper > 1 || (x.range.upper == 1 && x.range.upper_closed)
                               : x.range.lower < 0 || (x.range.lower == 0 && x.range.lower_closed)) ||
          (r.complement_dependent ? y.range.upper > 1 || (y.range.upper == 1 && y.range.upper_closed)
                                  : y.range.lower < 0 || (y.range.lower == 0 && y.range.lower_closed)))
        return MaxsetStatus::Invalid;
      break;
    default: return MaxsetStatus::Invalid;
  }
  return r.form == RelationForm::CountRatio || relation_intersects(r) ?
      MaxsetStatus::Ok : MaxsetStatus::Empty;
}

inline std::optional<Coordinates> box_witness(const Box& box, std::size_t dimension,
                                               bool upper) {
  Coordinates result;
  for (std::size_t i = 0; i < dimension; ++i) {
    const auto& axis = box.axes[i];
    const auto& r = axis.range;
    double value = upper ? r.upper : r.lower;
    const bool closed = upper ? r.upper_closed : r.lower_closed;
    if (std::isfinite(value) && !closed) {
      if (!upper && std::isfinite(r.upper) && !r.upper_closed)
        value = r.lower * .5 + r.upper * .5;
      if (!contains(axis, value))
        value = std::nextafter(upper ? r.upper : r.lower, upper ? -maxset_inf : maxset_inf);
    }
    if (r.lower < r.upper && std::isfinite(value) &&
        (upper ? r.upper_rounded : r.lower_rounded)) {
      const double inward = std::nextafter(value, upper ? -maxset_inf : maxset_inf);
      if (contains(axis, inward)) value = inward;
    }
    if (!std::isfinite(value)) {
      if (contains(axis, 0)) value = 0;
      else if (upper) value = r.lower_closed ? r.lower :
          std::max(std::nextafter(r.lower, maxset_inf), r.lower + std::max(1., std::abs(r.lower) * .5));
      else value = r.upper_closed ? r.upper :
          std::min(std::nextafter(r.upper, -maxset_inf), r.upper - std::max(1., std::abs(r.upper) * .5));
    }
    result.values[i] = value;
  }
  for (std::size_t pass = 0; pass < dimension; ++pass)
    for (std::size_t i = 0; i < box.order_size; ++i) {
      const auto o = box.order[i];
      auto& a = result.values[o.lhs];
      auto& b = result.values[o.rhs];
      if (a > b) {
        if (upper) a = b;
        else b = a;
      }
    }
  return contains(box, result, dimension) ? std::optional<Coordinates>(result) : std::nullopt;
}

inline std::optional<Coordinates> relation_witness(const Relation& r, std::size_t dimension,
                                                    double driver) {
  Coordinates result;
  for (std::size_t i = 0; i < dimension; ++i) result.values[i] = r.axes[i].range.lower;
  result.values[r.driver] = driver;
  result.values[r.dependent] = relation_solve(r, driver);
  for (std::size_t i = 0; i < dimension; ++i)
    if (!contains(r.axes[i], result.values[i])) return std::nullopt;
  return result;
}

struct MaximizerSet {
  std::array<Piece, 3> pieces{};
  std::uint8_t dimension = 0, size = 0;

  bool valid_dimension() const { return dimension > 0 && dimension <= maxset_dim; }

  MaxsetStatus add_points(basic_span<Point> source) {
    if (!valid_dimension()) return MaxsetStatus::Invalid;
    if (source.empty()) return MaxsetStatus::Empty;
    Points points;
    if (source.size() > points.values.size() || size >= pieces.size())
      return MaxsetStatus::Capacity;
    for (auto point : source) {
      for (std::size_t i = 0; i < dimension; ++i) {
        auto& x = point.estimate.values[i];
        if (!std::isfinite(x)) return MaxsetStatus::Invalid;
        x = maxset_zero(x);
        if (point.enclosure) {
          auto& a = point.enclosure->lower.values[i];
          auto& b = point.enclosure->upper.values[i];
          if (!std::isfinite(a) || !std::isfinite(b) || !(a <= x && x <= b))
            return MaxsetStatus::Invalid;
          a = maxset_zero(a);
          b = maxset_zero(b);
        }
      }
      for (std::size_t i = dimension; i < maxset_dim; ++i) {
        point.estimate.values[i] = 0;
        if (point.enclosure)
          point.enclosure->lower.values[i] = point.enclosure->upper.values[i] = 0;
      }
      bool duplicate = false;
      const auto check = [&](const Points& existing) {
        if (existing.size > existing.values.size()) return false;
        for (std::size_t j = 0; j < existing.size; ++j) {
          const auto& other = existing.values[j];
          if (!maxset_overlap(point, other, dimension)) continue;
          if (point.enclosure || other.enclosure) return false;
          duplicate = true;
        }
        return true;
      };
      if (!check(points)) return MaxsetStatus::Invalid;
      for (std::size_t k = 0; k < size; ++k)
        if (const auto* existing = std::get_if<Points>(&pieces[k].value))
          if (!check(*existing)) return MaxsetStatus::Invalid;
      if (!duplicate) points.values[points.size++] = point;
    }
    if (!points.size) return MaxsetStatus::Ok;
    maxset_sort(points.values, points.size,
        [](const Point& a, const Point& b) { return a.estimate.values < b.estimate.values; });
    pieces[size++].value = points;
    return MaxsetStatus::Ok;
  }

  MaxsetStatus add_box(Box box) {
    if (!valid_dimension()) return MaxsetStatus::Invalid;
    const auto status = normalize(box, dimension);
    if (status != MaxsetStatus::Ok) return status;
    if (size >= pieces.size()) return MaxsetStatus::Capacity;
    pieces[size++].value = box;
    return MaxsetStatus::Ok;
  }

  MaxsetStatus add_relation(Relation relation) {
    if (!valid_dimension()) return MaxsetStatus::Invalid;
    const auto status = normalize(relation, dimension);
    if (status != MaxsetStatus::Ok) return status;
    if (size >= pieces.size()) return MaxsetStatus::Capacity;
    pieces[size++].value = relation;
    return MaxsetStatus::Ok;
  }

  // One isolated point, including an enclosed root, or a union whose
  // fully fixed pieces all denote the same vector.
  bool singleton() const {
    if (!valid_dimension() || !size || size > pieces.size()) return false;
    std::optional<Coordinates> first;
    for (std::size_t k = 0; k < size; ++k) {
      const auto& piece = pieces[k].value;
      Coordinates point;
      if (const auto* points = std::get_if<Points>(&piece)) {
        if (points->size != 1) return false;
        point = points->values[0].estimate;
      } else if (const auto* box = std::get_if<Box>(&piece)) {
        for (std::size_t i = 0; i < dimension; ++i) {
          const auto& r = box->axes[i].range;
          if (r.lower != r.upper || r.lower_rounded || r.upper_rounded) return false;
          point.values[i] = r.lower;
        }
      } else if (const auto* relation = std::get_if<Relation>(&piece)) {
        const auto& x = relation->axes[relation->driver].range;
        if (x.lower != x.upper || x.lower_rounded || x.upper_rounded) return false;
        const auto root = relation_witness(*relation, dimension, x.lower);
        if (!root) return false;
        point = *root;
      }
      if (first && first->values != point.values) return false;
      first = point;
    }
    return true;
  }

  bool contains(Coordinates coordinates) const {
    if (!valid_dimension() || size > pieces.size()) return false;
    for (std::size_t k = 0; k < size; ++k) {
      const auto& piece = pieces[k].value;
      if (const auto* points = std::get_if<Points>(&piece)) {
        if (points->size > points->values.size()) return false;
        for (std::size_t j = 0; j < points->size; ++j) {
          const auto& point = points->values[j];
          bool match = true;
          for (std::size_t i = 0; i < dimension; ++i) {
            const double x = coordinates.values[i];
            match = match && (point.enclosure ?
                point.enclosure->lower.values[i] <= x && x <= point.enclosure->upper.values[i] :
                point.estimate.values[i] == x);
          }
          if (match) return true;
        }
      } else if (const auto* box = std::get_if<Box>(&piece)) {
        if (cppunivariate::contains(*box, coordinates, dimension)) return true;
      } else if (const auto* relation = std::get_if<Relation>(&piece)) {
        if (relation->driver >= dimension || relation->dependent >= dimension) return false;
        bool match = true;
        for (std::size_t i = 0; i < dimension; ++i)
          match = match && cppunivariate::contains(relation->axes[i], coordinates.values[i]);
        if (!match) continue;
        const double expected = relation_solve(*relation, coordinates.values[relation->driver]);
        const double observed = coordinates.values[relation->dependent];
        if (expected == observed) return true;
        if (relation->form != RelationForm::CountRatio && std::isfinite(expected)) {
          double bound = expected;
          for (int i = 0; i < 8; ++i) bound = std::nextafter(bound, observed);
          if (bound == observed) return true;
        }
      }
    }
    return false;
  }

  MaxsetStatus seal() {
    return slice({});
  }

  MaxsetStatus slice(const std::array<std::optional<double>, maxset_dim>& holds) {
    if (!valid_dimension() || size > pieces.size()) return MaxsetStatus::Invalid;
    for (std::size_t i = 0; i < dimension; ++i)
      if (holds[i] && !std::isfinite(*holds[i])) return MaxsetStatus::Invalid;
    MaximizerSet result;
    result.dimension = dimension;
    for (std::size_t k = 0; k < size; ++k) {
      MaxsetStatus status = MaxsetStatus::Invalid;
      if (const auto* points = std::get_if<Points>(&pieces[k].value)) {
        if (!points->size || points->size > points->values.size()) return MaxsetStatus::Invalid;
        Points selected;
        for (std::size_t j = 0; j < points->size; ++j) {
          const auto& point = points->values[j];
          bool keep = true;
          for (std::size_t i = 0; i < dimension; ++i) {
            if (!holds[i]) continue;
            if (point.enclosure && point.enclosure->lower.values[i] != point.enclosure->upper.values[i] &&
                point.enclosure->lower.values[i] <= *holds[i] &&
                *holds[i] <= point.enclosure->upper.values[i]) return MaxsetStatus::Invalid;
            keep = keep && point.estimate.values[i] == *holds[i];
          }
          if (keep) selected.values[selected.size++] = point;
        }
        status = result.add_points({selected.values.data(), selected.size});
      } else if (const auto* box = std::get_if<Box>(&pieces[k].value)) {
        Box selected = *box;
        bool keep = true;
        for (std::size_t i = 0; i < dimension; ++i) {
          if (!holds[i]) continue;
          keep = keep && cppunivariate::contains(selected.axes[i], *holds[i]);
          selected.axes[i] = fixed(*holds[i], selected.axes[i].integer);
        }
        status = keep ? result.add_box(selected) : MaxsetStatus::Empty;
      } else if (const auto* relation = std::get_if<Relation>(&pieces[k].value)) {
        Relation selected = *relation;
        status = normalize(selected, dimension);
        if (status == MaxsetStatus::Empty) continue;
        if (status != MaxsetStatus::Ok) return status;
        bool keep = true;
        for (std::size_t i = 0; i < dimension; ++i) {
          if (!holds[i]) continue;
          keep = keep && cppunivariate::contains(selected.axes[i], *holds[i]);
          selected.axes[i] = fixed(*holds[i], selected.axes[i].integer);
        }
        if (!keep) status = MaxsetStatus::Empty;
        else if (holds[selected.driver]) {
          auto witness = relation_witness(selected, dimension, *holds[selected.driver]);
          Point point{};
          if (witness) {
            point.estimate = *witness;
            if (selected.form != RelationForm::CountRatio && !holds[selected.dependent]) {
              Enclosure enclosure{*witness, *witness};
              const double value = witness->values[selected.dependent];
              enclosure.lower.values[selected.dependent] = std::nextafter(value, -maxset_inf);
              enclosure.upper.values[selected.dependent] = std::nextafter(value, maxset_inf);
              point.enclosure = enclosure;
            }
          }
          status = result.add_points({&point, witness ? std::size_t{1} : std::size_t{0}});
        } else if (holds[selected.dependent]) {
          const double y = *holds[selected.dependent];
          double x = 0, other = 0;
          if (selected.form == RelationForm::Semicircle) {
            const double a = selected.anchors[0], b = selected.anchors[1];
            const double mid = a * .5 + b * .5;
            const double radius = b * .5 - a * .5;
            if (y > radius) continue;
            const double ratio = y / radius;
            const double offset = radius * std::sqrt((1 - ratio) * (1 + ratio));
            const double step = y * (ratio / (1 + offset / radius));
            x = offset < radius * .5 ? mid - offset : a + step;
            other = offset < radius * .5 ? mid + offset : b - step;
            if (ratio < 1 && x == other) return MaxsetStatus::Invalid;
          } else if (selected.form == RelationForm::CountRatio) {
            auto inverse = selected;
            std::swap(inverse.level.numerator, inverse.level.denominator);
            x = other = relation_solve(inverse, y);
          } else if (selected.form == RelationForm::AdjacentBinomial) {
            const double size = std::round(selected.anchors[0] +
                relation_level(selected) * ((1 - y) / y));
            if (relation_solve(selected, size) != y) continue;
            x = other = size;
          } else if (selected.form == RelationForm::PartOverSum) {
            const double level = relation_level(selected);
            x = other = y * (level / (1 - level));
          } else {
            const double factor = relation_level(selected) / (selected.complement_dependent ? 1 - y : y);
            x = other = selected.complement_driver ? 1 - factor : factor;
          }
          Points points;
          for (double driver : {x, other}) {
            if (!cppunivariate::contains(selected.axes[selected.driver], driver) ||
                (points.size && points.values[0].estimate.values[selected.driver] == driver)) continue;
            Point point;
            for (std::size_t i = 0; i < dimension; ++i)
              point.estimate.values[i] = selected.axes[i].range.lower;
            point.estimate.values[selected.driver] = driver;
            if (selected.form != RelationForm::CountRatio &&
                selected.form != RelationForm::AdjacentBinomial &&
                !(selected.form == RelationForm::Semicircle && x == other)) {
              Enclosure enclosure{point.estimate, point.estimate};
              enclosure.lower.values[selected.driver] = std::nextafter(driver, -maxset_inf);
              enclosure.upper.values[selected.driver] = std::nextafter(driver, maxset_inf);
              point.enclosure = enclosure;
            }
            points.values[points.size++] = point;
          }
          status = result.add_points({points.values.data(), points.size});
        } else status = result.add_relation(selected);
      }
      if (status != MaxsetStatus::Ok && status != MaxsetStatus::Empty) return status;
    }
    for (std::size_t i = 0; i < result.size;) {
      const auto* inner = std::get_if<Box>(&result.pieces[i].value);
      bool redundant = false;
      if (inner) for (std::size_t j = 0; j < result.size && !redundant; ++j) {
        const auto* outer = std::get_if<Box>(&result.pieces[j].value);
        if (!outer || i == j || outer->order_size > 0) continue;
        bool subset = true, identical = inner->order_size == 0;
        for (std::size_t k = 0; k < dimension; ++k) {
          const auto& a = inner->axes[k].range;
          const auto& b = outer->axes[k].range;
          const bool exact = !(a.lower_rounded || a.upper_rounded || b.lower_rounded || b.upper_rounded);
          subset = subset && exact && (a.lower > b.lower || (a.lower == b.lower &&
              (b.lower_closed || !a.lower_closed))) &&
              (a.upper < b.upper || (a.upper == b.upper && (b.upper_closed || !a.upper_closed))) &&
              (!outer->axes[k].integer || inner->axes[k].integer);
          identical = identical && a.lower == b.lower && a.upper == b.upper &&
              a.lower_closed == b.lower_closed && a.upper_closed == b.upper_closed &&
              inner->axes[k].integer == outer->axes[k].integer;
        }
        redundant = subset && (!identical || j < i);
      }
      if (redundant) {
        for (std::size_t j = i + 1; j < result.size; ++j) result.pieces[j - 1] = result.pieces[j];
        --result.size;
      } else ++i;
    }
    const auto axis_key = [](const Axis& axis) {
      const auto& r = axis.range;
      return std::tuple{r.lower, r.upper, r.lower_closed, r.upper_closed,
                        axis.integer, r.lower_rounded, r.upper_rounded};
    };
    const auto piece_less = [&](const Piece& a, const Piece& b) {
      if (a.value.index() != b.value.index()) return a.value.index() < b.value.index();
      if (const auto* points = std::get_if<Points>(&a.value)) {
        const auto* other = std::get_if<Points>(&b.value);
        return std::lexicographical_compare(points->values.begin(), points->values.begin() + points->size,
            other->values.begin(), other->values.begin() + other->size,
            [](const Point& first, const Point& second) {
              return first.estimate.values < second.estimate.values;
            });
      }
      const auto* box = std::get_if<Box>(&a.value);
      const auto* other_box = std::get_if<Box>(&b.value);
      const auto* relation = std::get_if<Relation>(&a.value);
      const auto* other_relation = std::get_if<Relation>(&b.value);
      const auto& left = box ? box->axes : relation->axes;
      const auto& right = other_box ? other_box->axes : other_relation->axes;
      for (std::size_t i = 0; i < dimension; ++i) {
        const auto l = axis_key(left[i]), r = axis_key(right[i]);
        if (l != r) return l < r;
      }
      if (box)
        return std::lexicographical_compare(box->order.begin(), box->order.begin() + box->order_size,
            other_box->order.begin(), other_box->order.begin() + other_box->order_size,
            [](CountOrder l, CountOrder r) { return std::tie(l.lhs, l.rhs) < std::tie(r.lhs, r.rhs); });
      const auto key = [](const Relation& r) {
        return std::tie(r.form, r.driver, r.dependent, r.level.value, r.level.numerator,
                        r.level.denominator, r.level.rounded, r.anchors,
                        r.complement_driver, r.complement_dependent);
      };
      return key(*relation) < key(*other_relation);
    };
    maxset_sort(result.pieces, result.size, piece_less);
    const auto end = std::unique(result.pieces.begin(), result.pieces.begin() + result.size,
        [&](const Piece& a, const Piece& b) { return !piece_less(a, b) && !piece_less(b, a); });
    result.size = static_cast<std::uint8_t>(end - result.pieces.begin());
    *this = result;
    return size ? MaxsetStatus::Ok : MaxsetStatus::Empty;
  }
};
}  // namespace cppunivariate
