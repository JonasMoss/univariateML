#pragma once

#include "../hyper/mle.hpp"
#include <cmath>
#include <limits>

namespace cppunivariate {

enum class NhyperStatus { Ok, Tie, Unidentified, InvalidInput, Infeasible };
struct NhyperResult {
  double n, m, r, logLik, tie;
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

// N and r are known design constants; NaN m requests estimation.
// L(m)/L(m-1) = prod m/(m-r) * (N-m+1-x+r)/(N-m+1).
// Each factor is nonincreasing in m, strictly so for r>0. Thus the
// likelihood is log concave on [r, N-max(x)+r], including its endpoints.
inline NhyperResult nhyper_mle(span x, double r, double N, double m) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  NhyperResult out{nan, m, r, nan, nan, 0, NhyperStatus::InvalidInput};
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
  out.n = N - out.m;
  long double ll = 0;
  for (std::size_t j = 0; j < c.value.size(); ++j)
    ll += c.count[j] * nhyper_log_pmf(c.value[j], out.n, out.m, r);
  out.logLik = static_cast<double>(ll);
  return out;
}
}  // namespace cppunivariate
