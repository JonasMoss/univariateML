#pragma once

#include "../hyper/mle.hpp"
#include <cmath>
#include <limits>

namespace cppunivariate {

enum class NhyperStatus { Ok, Tie, Unidentified, InvalidInput, Infeasible, NumericalFailure };
struct NhyperResult {
  double m, N, r, logLik, tie;
  int iterations;
  NhyperStatus status;
};

// Total draws until the r-th white ball. P(T=x) = r/x P(H_x=r).
// The hypergeometric saddle-point kernel avoids large log-gamma differences.
inline double nhyper_log_pmf(double x, double n, double m, double r) {
  const double neginf = -std::numeric_limits<double>::infinity();
  if (r == 0) return x == 0 ? 0 : neginf;
  if (!std::isfinite(x) || x != std::floor(x) || x < r || x > n + r)
    return neginf;
  return hyper_log_pmf(r, n + m, m, x) + std::log(r / x);
}

namespace nhyper_detail {
// Correct a rounded quotient using compensated integer product differences.
// Within the representable count range this prevents an off-by-one tail bound.
inline long double population_bound(double m, double b, double r) {
  long double q = std::floor(static_cast<long double>(m) * b / r);
  if (q > hyper_detail::kMaxCount) return q;
  while (hyper_detail::product_difference(m, b, q, r) < 0) --q;
  while (hyper_detail::product_difference(m, b, q + 1, r) >= 0) ++q;
  return q;
}
}  // namespace nhyper_detail

// NaN requests estimation. N held permits composition and stopping count
// free; m held permits a finite search with N free. Existing composition
// search with N and r held:
// L(m)/L(m-1) = prod m/(m-r) * (N-m+1-x+r)/(N-m+1).
// Each factor is nonincreasing in m, strictly so for r>0. Thus the
// likelihood is log concave on [r, N-max(x)+r], including its endpoints.
inline NhyperResult nhyper_mle(span x, double r, double N, double m) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  NhyperResult out{m, N, r, nan, nan, 0, NhyperStatus::InvalidInput};
  // New finite grids: N held alone, or m held with N free.
  if ((std::isnan(r) && std::isnan(m) && !std::isnan(N)) ||
      (std::isnan(N) && !std::isnan(m))) {
    if (x.size() == 0 || (!std::isnan(N) && !hyper_detail::is_count(N)) ||
        (!std::isnan(m) && !hyper_detail::is_count(m)) ||
        (!std::isnan(r) && !hyper_detail::is_count(r))) return out;
    for (double v : x) if (!hyper_detail::is_count(v)) return out;
    const auto c = hyper_detail::tabulate(x);
    out.status = NhyperStatus::Infeasible;
    if ((!std::isnan(N) && c.max > N) ||
        (!std::isnan(m) && !std::isnan(r) && r > m)) return out;
    if (c.max == 0) {
      if (!std::isnan(r) && r != 0) return out;
      out.r = 0;
      if (std::isnan(N)) out.N = m;
      if (std::isnan(m)) out.m = 0;
      out.logLik = 0; out.status = NhyperStatus::Ok;
      return out;
    }
    if (c.min == 0 || r == 0) return out;
    const bool fixed_N = !std::isnan(N);
    const double rlo = std::isnan(r) ? 1 : r;
    const double rhi = std::isnan(r) ? std::min(c.min, fixed_N ? N : m) : r;
    if (rlo > rhi || rhi > c.min) return out;
    // Preflight exact line lengths, stopping as soon as the budget is exceeded.
    long double size = 0;
    out.status = NhyperStatus::NumericalFailure;
    for (double rr = rlo; rr <= rhi; ++rr) {
      const long double lo = fixed_N ? rr : static_cast<long double>(m) + c.max - rr;
      const long double hi = fixed_N ? static_cast<long double>(N) - c.max + rr :
          std::max(lo, nhyper_detail::population_bound(m, c.max, rr));
      if (hi > hyper_detail::kMaxCount) return out;
      size += hi - lo + 1;
      if (2 * size > hyper_detail::kGridBudget) return out;
    }
    long double best = -std::numeric_limits<long double>::infinity();
    bool selected = false;
    for (int pass = 0; pass < 2; ++pass) {
      for (double rr = rlo; rr <= rhi; ++rr) {
        const double lo = fixed_N ? rr : static_cast<double>(static_cast<long double>(m) + c.max - rr);
        const double hi = fixed_N ? N - c.max + rr : static_cast<double>(std::max(
            static_cast<long double>(lo), nhyper_detail::population_bound(m, c.max, rr)));
        for (double t = lo; t <= hi; ++t) {
          const double mm = fixed_N ? t : m, nn = fixed_N ? N : t;
          hyper_detail::Sum ll;
          for (std::size_t j = 0; j < c.value.size(); ++j) {
            const long double v = c.value[j];
            ll.add(c.count[j] * (hyper_detail::grid_choose(v - 1, rr - 1) +
                hyper_detail::grid_choose(static_cast<long double>(nn) - v, mm - rr) -
                hyper_detail::grid_choose(nn, mm)));
          }
          ++out.iterations;
          if (!std::isfinite(ll.value())) return out;
          if (pass == 0) best = std::max(best, ll.value());
          else if (hyper_detail::grid_tied(ll.value(), best) &&
              (!selected || mm < out.m || (mm == out.m &&
               (nn < out.N || (nn == out.N && rr < out.r))))) {
            out.m = mm; out.N = nn; out.r = rr;
            out.logLik = static_cast<double>(ll.value()); selected = true;
          }
        }
      }
    }
    if (selected) out.status = NhyperStatus::Ok;
    return out;
  }
  if (std::isnan(r) && hyper_detail::is_count(N) &&
      hyper_detail::is_count(m) && m <= N && x.size() != 0) {
    for (double v : x) if (!hyper_detail::is_count(v)) return out;
    const auto c = hyper_detail::tabulate(x);
    if (c.max == 0) { out.r = 0; out.logLik = 0; out.status = NhyperStatus::Ok; return out; }
    out.status = NhyperStatus::Infeasible;
    const double lo = std::max(1.0, c.max - (N - m));
    const double hi = std::min(m, c.min);
    if (lo > hi) return out;
    // L(t)/L(t-1) = product (x-t+1)(m-t+1) /
    // ((t-1)(N-m-x+t)); each factor decreases in t.
    const auto peak = hyper_detail::unimodal_peak(lo, hi, false, [&](double t) {
      hyper_detail::Sum d;
      for (std::size_t j = 0; j < c.value.size(); ++j) {
        const long double v = c.value[j];
        const long double a = v - t + 1, b = static_cast<long double>(m) - t + 1;
        const long double den1 = t - 1, den2 = static_cast<long double>(N) - m - v + t;
        const long double z = hyper_detail::product_difference(a, b, den1, den2) / (den1 * den2);
        if (std::fabs(z) < 0.5L) {
          d.add(c.count[j] * std::log1p(z));
        } else {
          d.add(c.count[j] * (std::log(a / den1) + std::log(b / den2)));
        }
      }
      return d;
    });
    out.r = peak.at; out.tie = peak.tie; out.iterations = peak.evaluations;
    out.status = std::isnan(peak.tie) ? NhyperStatus::Ok : NhyperStatus::Tie;
    long double ll = 0;
    for (std::size_t j = 0; j < c.value.size(); ++j)
      ll += c.count[j] * nhyper_log_pmf(c.value[j], N - m, m, out.r);
    out.logLik = static_cast<double>(ll);
    return out;
  }
  const bool free = std::isnan(m);
  if (x.size() == 0 || !hyper_detail::is_count(N) ||
      !hyper_detail::is_count(r) || r > N ||
      (!free && (!hyper_detail::is_count(m) || m < r || m > N))) return out;
  for (double v : x) if (!hyper_detail::is_count(v)) return out;
  const auto c = hyper_detail::tabulate(x);
  out.status = NhyperStatus::Infeasible;
  if (r == 0) {
    if (c.max != 0) return out;
    if (free) { out.m = 0; out.tie = N; }
    out.status = free && N > 0 ? NhyperStatus::Unidentified : NhyperStatus::Ok;
  } else {
    if (c.min < r || c.max > N) return out;
    const double hi = N - c.max + r;
    if (!free && m > hi) return out;
    if (free) {
      const auto peak = hyper_detail::unimodal_peak(r, hi, false, [&](double t) {
        hyper_detail::Sum d;
        for (std::size_t j = 0; j < c.value.size(); ++j) {
          const long double failures = static_cast<long double>(N) - t + 1;
          const long double y = c.value[j] - r;
          // Combine before taking logs to retain adjacent-integer signs.
          const long double z = hyper_detail::product_difference(
              r, failures, t, y) / ((t - r) * failures);
          d.add(c.count[j] * std::log1p(z));
        }
        return d;
      });
      out.m = peak.at; out.tie = peak.tie; out.iterations = peak.evaluations;
      out.status = std::isnan(peak.tie) ? NhyperStatus::Ok : NhyperStatus::Tie;
    } else out.status = NhyperStatus::Ok;
  }
  long double ll = 0;
  for (std::size_t j = 0; j < c.value.size(); ++j)
    ll += c.count[j] * nhyper_log_pmf(c.value[j], out.N - out.m, out.m, r);
  out.logLik = static_cast<double>(ll);
  return out;
}
}  // namespace cppunivariate
