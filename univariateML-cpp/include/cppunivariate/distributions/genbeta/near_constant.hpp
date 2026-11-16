// Generalized beta, scale held, all of p, q, a free, on a near-constant sample.
//
// With y = log(scale / x) the sample has relative spread Dy / mean(y) below 1e-6,
// so y rounded to binary64 has no usable spread (one ulp of y is as large as
// the differences). The data are used as offsets from the largest observation,
// d_i = log(x_max / x_i), exact to rounding. There is no finite maximizer. The
// supremum is one of two faces, and which one depends on the sample:
//
//   power  (a -> infinity):  n (-log g - 1) + sum(-log x_i),  g = mean(d_i)
//   lgamma (a -> 0, p -> infinity, p a -> lambda):  the gamma fit of y, plus sum(-log x_i)
//
// To leading order their difference is n (log(sqrt(2 pi) s / g) - 1/2) with s the
// standard deviation of y, so the power face is higher when s / g > sqrt(e / (2 pi))
// = 0.6577 and the lgamma face otherwise. Both values are evaluated, with error far
// below their difference unless the sample sits on that threshold, in which case
// nothing is decided. Finite (p, q, a) were scanned numerically (dev/research/evaluations/
// 2026-11-16-near-constant-limits-report.md), not excluded by a bound: the outcome is a
// limit that is not certified.
#pragma once
#include "../gamma/relative.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace cppunivariate { namespace genbeta_detail {

inline bool near_constant_limit(span x, double scale, GenbetaResult& out) {
  const std::size_t count = x.size();
  if (count < 2 || !(scale > 0) || !std::isfinite(scale)) return false;
  double lo = std::numeric_limits<double>::infinity(), hi = -lo;
  for (double v : x) {
    if (!(v > 0 && v < scale) || !std::isfinite(v)) return false;
    lo = std::min(lo, v); hi = std::max(hi, v);
  }
  if (!(hi > lo) || !((hi - lo) / lo <= 1e-6)) return false;
  const double n = static_cast<double>(count);
  // y at the largest observation (the smallest y), and offsets d_i >= 0.
  const double y0 = hi > scale * .5 ? -std::log1p((hi - scale) / scale) : std::log(scale) - std::log(hi);
  std::vector<double> d(count);
  relative_gamma::Sum dsum;
  for (std::size_t i = 0; i < count; ++i) { d[i] = std::log1p((hi - x[i]) / x[i]); dsum.add(d[i]); }
  const double gap = dsum.value() / n;
  const double ybar = y0 + gap;
  if (!(gap > 0) || !(y0 > 0) || !(gap <= 1e-6 * ybar)) return false;
  std::vector<double> v(count);
  relative_gamma::Sum vsum;
  for (std::size_t i = 0; i < count; ++i) { v[i] = (d[i] - gap) / ybar; vsum.add(v[i]); }
  const double vbar = vsum.value() / n;
  const double c = relative_gamma::log_ratio(v);
  if (!(c > 0) || !(c < 1e-3)) return false;
  const double q = relative_gamma::shape_from_ratio(c);
  // Mean of y, the log of it, and -sum(log x) = n (mean(y) - log scale).
  const double ytrue = ybar * (1 + vbar), logy = std::log(ybar) + std::log1p(vbar);
  const double jac = n * (ytrue - std::log(scale));
  const double ll_gamma = n * (relative_gamma::loglik_core(q, c) - logy) + jac;
  const double ll_power = n * (-std::log(gap) - 1) + jac;
  if (!std::isfinite(ll_gamma) || !std::isfinite(ll_power)) return false;
  // The difference without the shared terms, so that it keeps its own accuracy.
  const double diff = n * (-std::log(gap) - 1 - relative_gamma::loglik_core(q, c) + logy);
  const double err = 4096 * std::numeric_limits<double>::epsilon() * (1 + std::abs(ll_gamma) + std::abs(jac));
  if (!(std::abs(diff) > 4 * err)) return false;
  const bool power = diff > 0;
  const std::vector<double> coef_power{hi, 1 / gap}, coef_gamma{q, ytrue / q, scale};
  if (!valid_stop_coefficients(StopModel::Power, coef_power) || !valid_stop_coefficients(StopModel::UnitGamma, coef_gamma)) return false;
  const double win = power ? ll_power : ll_gamma, lose = power ? ll_gamma : ll_power;
  StopDetail detail;
  detail.limit_model = power ? StopModel::Power : StopModel::UnitGamma;
  detail.limit_coef = power ? coef_power : coef_gamma;
  detail.limit_loglik = win;
  detail.candidates.push_back({detail.limit_coef, win - err, win + err, detail.limit_model});
  detail.candidates.push_back({power ? coef_gamma : coef_power, lose - err, lose + err, power ? StopModel::UnitGamma : StopModel::Power});
  detail.loglik_lower = win - err;
  detail.loglik_upper = std::numeric_limits<double>::infinity();
  out = GenbetaResult{NAN, NAN, NAN, scale, win, out.iterations, power ? GenbetaStatus::PowerBoundary : GenbetaStatus::UnitGammaBoundary};
  out.stop_detail = detail;
  out.allfree_certified = false;
  return true;
}

}}  // namespace cppunivariate::genbeta_detail
