// Gamma face of a sample known through relative offsets.
//
// A sample x_i = x0 (1 + v_i) with every |v_i| far below 1 cannot be handed to
// a logarithm: log(x_i) rounds each point by about 1e-16, which is as large as
// the spread v_i itself when the sample is near-constant. The two quantities a
// gamma fit needs are functions of the offsets alone.
#pragma once
#include "mle.hpp"
#include <cmath>
#include <vector>

namespace cppunivariate { namespace relative_gamma {

// Neumaier compensated sum.
struct Sum {
  double s = 0, c = 0;
  void add(double v) {
    const double t = s + v;
    if (std::abs(s) >= std::abs(v)) c += (s - t) + v; else c += (v - t) + s;
    s = t;
  }
  double value() const { return s + c; }
};

// log(mean(1 + v)) - mean(log(1 + v)) = sum_{k>=2} (-1)^k (mean(v^k) - vbar^k) / k.
// The linear terms cancel exactly because both means are of the same v. Needs
// max |v| <= 1e-3 (the k = 9 term is then below 1e-27 times the k = 2 term).
inline double log_ratio(const std::vector<double>& v) {
  const double n = static_cast<double>(v.size());
  Sum m[10];
  for (double t : v) {
    double p = 1;
    for (int k = 1; k <= 9; ++k) { p *= t; m[k].add(p); }
  }
  const double vbar = m[1].value() / n;
  double c = 0, pbar = 1;
  for (int k = 1; k <= 9; ++k) {
    pbar *= vbar;
    if (k >= 2) c += ((k % 2 == 0) ? 1.0 : -1.0) * (m[k].value() / n - pbar) / k;
  }
  return c;
}

// The root of log(a) - digamma(a) = c for small c: the Stirling series
// 1/(2a) + 1/(12 a^2) - 1/(120 a^4) + 1/(252 a^6), solved by fixed point.
// Relative error below 1e-15 for c <= 1e-3 (a >= 500).
inline double shape_from_ratio(double c) {
  double a = 1 / (2 * c);
  for (int j = 0; j < 12; ++j) {
    const double r = 1 / (a * a);
    a = 1 / (2 * (c - r * (1.0 / 12 - r * (1.0 / 120 - r / 252))));
  }
  return a;
}

// Gamma log likelihood per observation at the MLE, without the -log(mean)
// term: a log a - a - lgamma(a) - (a - 1) c.
inline double loglik_core(double a, double c) {
  return 0.5 * std::log(a / (2 * 3.141592653589793238462643383279502884)) - gamma_detail::stirlerr(a) - a * c + c;
}

// Double-double values, for the one quantity that no double carries: the ratio of the sample mean to
// shape * scale at rounded coefficients. With a shape of 1e32 a relative rounding of the scale of 1e-16
// costs shape * (1e-16)^2 / 2, about 1 in the log likelihood.
struct DD { double hi, lo; };
inline DD two_sum(double a, double b) {
  const double s = a + b, bb = s - a;
  return {s, (a - (s - bb)) + (b - bb)};
}
inline DD quick_sum(double a, double b) { const double s = a + b; return {s, b - (s - a)}; }
inline DD dd_add(DD a, DD b) {
  const DD s = two_sum(a.hi, b.hi), t = two_sum(a.lo, b.lo);
  const DD u = quick_sum(s.hi, s.lo + t.hi);
  return quick_sum(u.hi, t.lo + u.lo);
}
inline DD dd_neg(DD a) { return {-a.hi, -a.lo}; }
inline DD two_prod(double a, double b) { const double p = a * b; return {p, std::fma(a, b, -p)}; }
inline DD dd_div_d(DD a, double b) {
  const double q1 = a.hi / b;
  DD r = dd_add(a, dd_neg(two_prod(q1, b)));
  const double q2 = r.hi / b;
  r = dd_add(r, dd_neg(two_prod(q2, b)));
  const double q3 = r.hi / b;
  return dd_add(quick_sum(q1, q2), {q3, 0});
}
// Mean of f(x_i) for f = identity or reciprocal, to about 1e-32 relative.
inline DD dd_mean(const double* x, std::size_t n, bool reciprocal) {
  DD sum{0, 0};
  for (std::size_t i = 0; i < n; ++i) sum = dd_add(sum, reciprocal ? dd_div_d({1, 0}, x[i]) : DD{x[i], 0});
  return dd_div_d(sum, static_cast<double>(n));
}
inline DD dd_mul_d(DD a, double b) {
  const DD p = two_prod(a.hi, b);
  return quick_sum(p.hi, p.lo + a.lo * b);
}
// a (log(1 + eta) - eta), the loss per observation of a gamma log likelihood when the scale is off its
// maximizer by the factor 1 + eta, with eta = mean / (shape * scale) - 1 = mean * rate / shape - 1
// computed in double-double. Pass the scale, or (rate = true) the rate 1 / scale.
inline double scale_loss(DD mean, double shape, double scale, bool rate = false) {
  const DD product = rate ? dd_mul_d(mean, scale) : DD{two_prod(shape, scale)};
  const DD target = rate ? DD{shape, 0} : mean;
  const DD gap = dd_add(target, dd_neg(product));
  const double eta = (rate ? -1.0 : 1.0) * (gap.hi + gap.lo) / (rate ? shape : product.hi);
  const double term = std::abs(eta) < 1e-3
      ? -eta * eta * (0.5 - eta * (1.0 / 3 - eta * (1.0 / 4 - eta / 5)))
      : std::log1p(eta) - eta;
  return shape * term;
}

}}  // namespace cppunivariate::relative_gamma
