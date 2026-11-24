#pragma once

#include "dpqr.hpp"
#include "../pareto/dpqr.hpp"
#include "../power/dpqr.hpp"
#include "../../core/maximizer_set.hpp"
#include "../../core/precision.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/transformed_data.hpp"

namespace cppunivariate {

enum class DblparetoStatus { Ok, InvalidInput, Unbounded, ParetoLimit, PowerLimit, Evaluation };

struct DblparetoResult {
  double scale = NAN, left_rate = NAN, right_rate = NAN, logLik = NAN;
  DblparetoStatus status = DblparetoStatus::InvalidInput;
  bool certified = true;
  MaximizerSet maximizers;
  StopDetail stop_detail;
};

inline FitOutcome outcome(const DblparetoResult& r) {
  switch (r.status) {
    case DblparetoStatus::Ok:
      return r.certified ? FitOutcome::estimate(true, !r.maximizers.size)
                         : FitOutcome::undetermined();
    case DblparetoStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case DblparetoStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case DblparetoStatus::ParetoLimit:
    case DblparetoStatus::PowerLimit: return FitOutcome::limit(r.certified);
    case DblparetoStatus::Evaluation: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}

namespace dblpareto_detail {

struct Profile {
  double scale = NAN;
  ExtendedReal left = NAN, right = NAN, loss = INFINITY;
};

inline ExtendedReal log_add(ExtendedReal a, ExtendedReal b) {
  const auto larger = std::max(a, b), smaller = std::min(a, b);
  return larger == -INFINITY ? larger : larger + std::log1p(std::exp(smaller - larger));
}

inline Profile profile(double scale, ExtendedReal l, ExtendedReal r,
                       double left, double right) {
  if (std::isnan(left) && std::isnan(right)) {
    const auto s = std::sqrt(l) + std::sqrt(r);
    return {scale, 1 / (std::sqrt(l) * s), 1 / (std::sqrt(r) * s), std::log(s)};
  }
  const bool free_left = std::isnan(left);
  const auto gap = free_left ? l : r, other = free_left ? r : l;
  const ExtendedReal held = free_left ? right : left;
  const auto inverse = gap / 2 + std::hypot(gap / 2, std::sqrt(gap) / std::sqrt(held));
  const auto log_inverse = std::log(inverse), log_held = std::log(held);
  // Compare logarithms of losses relative to log(held). All three terms
  // are nonnegative, and their sum can be outside the binary64 range.
  const auto log_ratio = log_inverse + log_held;
  const auto normalization = log_ratio < -36 ? log_ratio :
      std::log(std::max(log_ratio, ExtendedReal{0}) + std::log1p(std::exp(-std::abs(log_ratio))));
  const auto penalty = gap == 0 ? -INFINITY : std::log(gap) - log_inverse;
  const auto loss = log_add(log_add(normalization, penalty), log_held + std::log(other));
  const auto rate = 1 / inverse;
  return free_left ? Profile{scale, rate, held, loss} : Profile{scale, held, rate, loss};
}

// Compare exact products of positive binary64 numbers without range loss.
inline int compare_products(double a, double b, double c, double d) {
  int ea, eb, ec, ed;
  a = std::frexp(a, &ea); b = std::frexp(b, &eb);
  c = std::frexp(c, &ec); d = std::frexp(d, &ed);
  const int difference = ea + eb - ec - ed;
  if (difference > 1) return 1;
  if (difference < -1) return -1;
  a = std::ldexp(a, difference);
  const double first = a * b, second = c * d;
  if (first != second) return first > second ? 1 : -1;
  const double error = std::fma(a, b, -first) - std::fma(c, d, -second);
  return (error > 0) - (error < 0);
}

inline double log_likelihood(span x, double scale, double left, double right) {
  transformed_detail::Sum sum;
  for (double v : x) sum.push(distributions::ddblpareto(v, scale, left, right, true));
  return sum.get();
}

} // namespace dblpareto_detail

inline DblparetoResult dblpareto_mle(span x, double scale = NAN, double left = NAN,
                                    double right = NAN) {
  DblparetoResult out;
  if (x.empty()) return out;
  for (double v : {scale, left, right})
    if (!std::isnan(v) && (!(v > 0) || !std::isfinite(v))) return out;
  double lo = INFINITY, hi = 0;
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v)) return out;
    lo = std::min(lo, v);
    hi = std::max(hi, v);
  }
  const bool free_scale = std::isnan(scale), free_left = std::isnan(left),
             free_right = std::isnan(right);
  const double n = static_cast<double>(x.size());
  if (lo == hi && (free_scale || scale == lo) && free_left && free_right) {
    out.status = DblparetoStatus::Unbounded;
    return out;
  }

  dblpareto_detail::Profile best;
  if (!free_left && !free_right) {
    if (free_scale) {
      std::vector<double> sorted(x.begin(), x.end());
      std::sort(sorted.begin(), sorted.end());
      // First split where left_rate*k >= right_rate*(n-k).
      std::size_t low = 1, high = sorted.size();
      const auto slope = [&](std::size_t k) {
        return k == sorted.size() ? 1 :
          dblpareto_detail::compare_products(left, static_cast<double>(k),
                                             right, n - static_cast<double>(k));
      };
      while (low < high) {
        const auto mid = low + (high - low) / 2;
        if (slope(mid) < 0) low = mid + 1;
        else high = mid;
      }
      scale = sorted[low - 1];
      if (slope(low) == 0 && scale != sorted[low]) {
        Box interval_set;
        interval_set.axes[0] = interval(scale, sorted[low], true, true);
        interval_set.axes[1] = fixed(left);
        interval_set.axes[2] = fixed(right);
        out.maximizers.dimension = 3;
        out.maximizers.add_box(interval_set);
      }
    }
    best = {scale, left, right, 0};
  } else if (!free_scale) {
    transformed_detail::Sum l, r;
    for (double v : x) {
      if (v < scale) l.push(transformed_detail::log_ratio(scale, v));
      if (v > scale) r.push(transformed_detail::log_ratio(v, scale));
    }
    best = dblpareto_detail::profile(scale, l.get() / n, r.get() / n, left, right);
  } else {
    std::vector<double> sorted(x.begin(), x.end());
    std::sort(sorted.begin(), sorted.end());
    std::vector<ExtendedReal> right_gaps(sorted.size());
    transformed_detail::Sum r, l;
    for (std::size_t i = sorted.size() - 1; i > 0; --i) {
      r.push(((n - static_cast<double>(i)) / n) *
             transformed_detail::log_ratio(sorted[i], sorted[i - 1]));
      right_gaps[i - 1] = r.get();
    }
    std::vector<dblpareto_detail::Profile> profiles;
    for (std::size_t i = 0; i < sorted.size(); ++i) {
      if (i) {
        if (sorted[i] == sorted[i - 1]) continue;
        l.push((static_cast<double>(i) / n) *
               transformed_detail::log_ratio(sorted[i], sorted[i - 1]));
      }
      profiles.push_back(dblpareto_detail::profile(sorted[i], l.get(), right_gaps[i],
                                                  left, right));
    }
    best = *std::min_element(profiles.begin(), profiles.end(),
                             [](const auto& a, const auto& b) { return a.loss < b.loss; });
    const auto close = [&](const auto& p) {
      const auto margin = 128 * std::numeric_limits<double>::epsilon() *
          (1 + std::abs(best.loss) + std::abs(p.loss));
      return p.loss == best.loss || (std::isfinite(best.loss) && std::isfinite(p.loss) &&
                                     std::abs(p.loss - best.loss) <= margin);
    };
    const auto finite = [&](const auto& p) {
      return std::isfinite(p.left) && std::isfinite(p.right);
    };
    // In an unresolved face comparison prefer the accurate finite witness.
    if (!finite(best)) {
      dblpareto_detail::Profile nearby;
      for (const auto& p : profiles)
        if (finite(p) && close(p) && p.loss < nearby.loss) nearby = p;
      if (finite(nearby)) best = nearby;
    }
    out.certified = std::none_of(profiles.begin(), profiles.end(),
        [&](const auto& p) { return p.scale != best.scale && close(p); });

    // A reciprocal reflection exchanges the rate profiles exactly.
    if (!out.certified && free_left && free_right) {
      bool reflected = true;
      for (std::size_t i = 1; i < sorted.size(); ++i)
        if (dblpareto_detail::compare_products(sorted[i], sorted[sorted.size() - 1 - i],
                sorted[i - 1], sorted[sorted.size() - i]) != 0) {
          reflected = false;
          break;
        }
      if (reflected) {
        double mirror = NAN;
        for (std::size_t i = 0; i < sorted.size(); ++i)
          if (sorted[i] == best.scale) { mirror = sorted[sorted.size() - 1 - i]; break; }
        if (mirror != best.scale && std::all_of(profiles.begin(), profiles.end(),
            [&](const auto& p) { return p.scale == best.scale || p.scale == mirror || !close(p); })) {
          out.certified = true;
          if (finite(best)) {
            std::array<Point, 2> points{};
            points[0].estimate.values = {best.scale, static_cast<double>(best.left),
                                         static_cast<double>(best.right)};
            points[1].estimate.values = {mirror, static_cast<double>(best.right),
                                         static_cast<double>(best.left)};
            for (auto& point : points) {
              auto lower = point.estimate, upper = point.estimate;
              for (unsigned j = 1; j < 3; ++j) {
                lower.values[j] *= 1 - 1e-11;
                upper.values[j] *= 1 + 1e-11;
              }
              point.enclosure = Enclosure{lower, upper};
            }
            out.maximizers.dimension = 3;
            if (out.maximizers.add_points({points.data(), points.size()}) != MaxsetStatus::Ok)
              out.certified = false;
          }
        }
      }
    }
  }

  out.scale = best.scale;
  out.left_rate = static_cast<double>(best.left);
  out.right_rate = static_cast<double>(best.right);
  out.status = DblparetoStatus::Evaluation;
  if (best.left == INFINITY || best.right == INFINITY) {
    const bool pareto = best.left == INFINITY;
    const auto rate = pareto ? out.right_rate : out.left_rate;
    if (!(rate > 0) || !std::isfinite(rate)) return out;
    transformed_detail::Sum sum;
    for (double v : x)
      sum.push(pareto ? distributions::dpareto(v, rate, out.scale, true)
                      : distributions::dpower(v, out.scale, rate, true));
    out.logLik = sum.get();
    out.stop_detail = pareto ? limit_detail({rate, out.scale}, out.logLik, StopModel::Pareto)
                             : limit_detail({out.scale, rate}, out.logLik, StopModel::Power);
    if (std::isfinite(out.logLik))
      out.status = pareto ? DblparetoStatus::ParetoLimit : DblparetoStatus::PowerLimit;
  } else if (distributions::dblpareto_parameters(out.scale, out.left_rate, out.right_rate)) {
    const auto accurate = [](ExtendedReal value, double stored) {
      return std::abs(value - stored) <= 1e-11 * value &&
             coefficient_spacing(stored, stored) <= 1e-11;
    };
    if ((free_left && !accurate(best.left, out.left_rate)) ||
        (free_right && !accurate(best.right, out.right_rate))) return out;
    out.logLik = dblpareto_detail::log_likelihood(x, out.scale, out.left_rate, out.right_rate);
    if (std::isfinite(out.logLik)) out.status = DblparetoStatus::Ok;
  }
  return out;
}

} // namespace cppunivariate
