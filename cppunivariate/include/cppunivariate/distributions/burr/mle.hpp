// Three-parameter Burr XII MLE, with shape1 profiled exactly.
#pragma once
#include "../../core/bfgs.hpp"
#include "../../core/finite_diff.hpp"
#include "../../core/span.hpp"
#include "../weibull/mle.hpp"
#include "../pareto/mle.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
inline double burr_softplus(double z) {
  return std::max(z, 0.0) + std::log1p(std::exp(-std::abs(z)));
}
// Fixed-scale cost supplies an additional legacy two-shape starting fit.
struct BurrCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double k = theta[0];
    const double c = theta[1];
    if (!(k > 0.0 && c > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1py = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      const double lx = std::log(v);
      S_lx += lx;
      S_l1py += burr_softplus(c * lx);
    }
    return -n * (std::log(k) + std::log(c)) - (c - 1.0) * S_lx +
           (k + 1.0) * S_l1py;
  }
  void gradient(const double *theta, double *g) const {
    const double k = theta[0];
    const double c = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1py = 0.0, S_y_lx_over_1py = 0.0;
    for (auto v : x) {
      const double lx = std::log(v);
      const double z = c * lx;
      if (z > 350.0) {
        S_lx += lx;
        S_l1py += burr_softplus(z);
        S_y_lx_over_1py += lx / (1.0 + std::exp(-z));
        continue;
      }
      const double y = std::exp(z);
      S_lx += lx;
      S_l1py += std::log1p(y);
      S_y_lx_over_1py += y * lx / (1.0 + y);
    }
    g[0] = -n / k + S_l1py;
    g[1] = -n / c - S_lx + (k + 1.0) * S_y_lx_over_1py;
  }
  void hessian(const double *theta, double *H) const {
    const double k = theta[0];
    const double c = theta[1];
    const double n = static_cast<double>(x.size());
    double H_cc_acc = 0.0, H_kc_acc = 0.0;
    for (auto v : x) {
      const double lx = std::log(v);
      const double z = c * lx;
      if (z > 350.0) {
        const double e = std::exp(-z);
        H_cc_acc += (k + 1.0) * lx * lx * e / ((1.0 + e) * (1.0 + e));
        H_kc_acc += lx / (1.0 + e);
        continue;
      }
      const double y = std::exp(z);
      const double yp1 = 1.0 + y;
      H_cc_acc += (k + 1.0) * y * lx * lx / (yp1 * yp1);
      H_kc_acc += y * lx / yp1;
    }
    H[0] = n / (k * k);
    H[1] = H_kc_acc;
    H[2] = H_kc_acc;
    H[3] = n / (c * c) + H_cc_acc;
  }
};


enum class BurrStatus { Ok = 0, WeibullBoundary, ParetoBoundary, InvalidInit, IterationFailure };
struct BurrMLE {
  double shape1, shape2, scale, logLik;
  int iterations;
  BurrStatus status;
};
// theta = (log shape2, log scale relative to geometric mean).
// shape1 = n / sum softplus(shape2 * log(x/scale)).
// Writing z-softplus(z) = -softplus(-z) avoids tail cancellation.
struct BurrProfileCost {
  span logs;
  int dim() const { return 2; }
  double shape1(const double *theta) const {
    double total = 0.0;
    for (double l : logs) total += burr_softplus(std::exp(theta[0]) * (l-theta[1]));
    return static_cast<double>(logs.size()) / total;
  }
  double operator()(const double *theta) const {
    const double c = std::exp(theta[0]);
    const double k = shape1(theta);
    if (!(c > 0.0 && k > 0.0) || !std::isfinite(c) || !std::isfinite(k))
      return std::numeric_limits<double>::infinity();
    double value = 1.0 - std::log(k) - theta[0];
    const double n = static_cast<double>(logs.size());
    for (double l : logs) value += (l + burr_softplus(-c*(l-theta[1]))) / n;
    return value;
  }
};
inline BurrMLE burr_mle(span x, double tol, int maxit,
    double shape10 = std::numeric_limits<double>::quiet_NaN(),
    double shape20 = std::numeric_limits<double>::quiet_NaN(),
    double scale0 = std::numeric_limits<double>::quiet_NaN(), double reltol = 1e-12) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (x.size() < 2) return {nan,nan,nan,nan,0,BurrStatus::InvalidInit};
  std::vector<double> logs;
  double center = 0.0;
  for (double v : x) {
    if (!(v > 0.0) || !std::isfinite(v)) return {nan,nan,nan,nan,0,BurrStatus::InvalidInit};
    logs.push_back(std::log(v)); center += logs.back()/static_cast<double>(x.size());
  }
  double variance = 0.0;
  for (double &l : logs) { l -= center; variance += l*l/static_cast<double>(x.size()); }
  if (!(variance > 0.0)) return {nan,nan,nan,nan,0,BurrStatus::InvalidInit};
  for (double start : {shape10, shape20, scale0})
    if (!std::isnan(start) && (!(start > 0.0) || !std::isfinite(start)))
      return {nan,nan,nan,nan,0,BurrStatus::InvalidInit};
  BurrProfileCost cost{span{logs.data(),logs.size()}};
  finite_diff_cost<BurrProfileCost> fd{cost};
  BfgsControl ctl;
  ctl.grad_tol = tol; ctl.reltol = reltol; ctl.maxit = maxit;
  BfgsResult best;
  best.f = std::numeric_limits<double>::infinity();
  best.iterations = 0;
  const double unit = std::sqrt(variance);
  // Match log moments at a grid of shape1 values, including log-logistic.
  // Extra endpoint starts probe the two different boundary basins.
  for (double k : {0.01,0.05,0.2,0.5,1.0,2.0,10.0,100.0}) {
    const double c = std::sqrt((M_PI*M_PI/6.0 + special::trigamma(k))/variance);
    const double loc = (special::digamma(k) + 0.5772156649015328606)/c;
    const double start[2] = {std::log(c),loc};
    auto r = bfgs_minimize(fd,start,ctl);
    if (std::isfinite(r.f) && r.f < best.f) best = r;
  }
  // Fit the old two-shape family after centering log data. Its scale is
  // the geometric mean, so the seed is invariant to measurement units.
  std::vector<double> normalized;
  bool representable = true;
  for (double l : logs) {
    normalized.push_back(std::exp(l));
    if (!(normalized.back() > 0.0) || !std::isfinite(normalized.back())) representable = false;
  }
  if (representable) {
    BurrCost fixed{span{normalized.data(),normalized.size()}};
    const double fixed_start[2] = {1.0,1.0/unit};
    auto seed = bfgs_minimize(fixed,fixed_start,ctl);
    if (std::isfinite(seed.f) && seed.theta[1] > 0.0) {
      const double start[2] = {std::log(seed.theta[1]),0.0};
      auto r = bfgs_minimize(fd,start,ctl);
      if (std::isfinite(r.f) && r.f < best.f) best = r;
    }
  }
  if (!std::isnan(shape10) || !std::isnan(shape20) || !std::isnan(scale0)) {
    const double k = std::isnan(shape10) ? 1.0 : shape10;
    const double c = std::isnan(shape20) ? 1.0/unit : shape20;
    const double start[2] = {std::log(c), std::isnan(scale0) ?
      (special::digamma(k)+0.5772156649015328606)/c : std::log(scale0)-center};
    auto r = bfgs_minimize(fd,start,ctl);
    if (std::isfinite(r.f) && r.f < best.f) best = r;
  }
  const double n = static_cast<double>(x.size());
  const double ll = -n*(best.f+center);
  const auto w = weibull_mle(x,1e-10,std::max(maxit,100));
  const auto p = pareto_mle(x);
  const double tie = 1e-8*n;
  if (w.logLik >= ll-tie && w.logLik >= p.logLik-tie)
    return {INFINITY,w.shape,w.scale,w.logLik,best.iterations,BurrStatus::WeibullBoundary};
  if (p.logLik >= ll-tie)
    return {0.0,INFINITY,p.min,p.logLik,best.iterations,BurrStatus::ParetoBoundary};
  if (!std::isfinite(best.f)) return {nan,nan,nan,nan,0,BurrStatus::IterationFailure};
  return {cost.shape1(best.theta.data()),std::exp(best.theta[0]),
    std::exp(center+best.theta[1]),ll,best.iterations,
    best.status == BfgsStatus::Ok ? BurrStatus::Ok : BurrStatus::IterationFailure};
}
} // namespace cppunivariate
