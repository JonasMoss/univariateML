// Exact inflation profile; conditional-mean solve for prob, scalar size search.
#pragma once
#include "../nbinom/mle.hpp"
#include "../zip/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <map>

namespace cppunivariate {
enum class ZinbStatus { Ok, InvalidInput, InvalidSize, IterationFailure, ZipLimit, PoissonLimit };
struct ZinbResult {
  double size, prob, pi, logLik;
  int iterations;
  ZinbStatus status;
};

inline ZinbResult zinb_mle(ispan x, double tol = 1e-10, int maxit = 200,
                         double fixed_size = std::numeric_limits<double>::quiet_NaN()) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  ZinbResult best{nan, nan, nan, -std::numeric_limits<double>::infinity(), 0,
                  ZinbStatus::IterationFailure};
  const bool fixed = !std::isnan(fixed_size);
  if (fixed && (!(fixed_size > 0) || !std::isfinite(fixed_size))) {
    best.status = ZinbStatus::InvalidSize; return best;
  }
  if (x.size() == 0 || !(tol > 0) || !std::isfinite(tol) || maxit < 1) {
    best.status = ZinbStatus::InvalidInput; return best;
  }
  std::map<int, int> counts;
  double sum = 0, zeros = 0;
  for (int v : x) {
    if (v < 0) { best.status = ZinbStatus::InvalidInput; return best; }
    ++counts[v]; sum += v; if (v == 0) ++zeros;
  }
  const double n = static_cast<double>(x.size()), z = zeros / n;
  if (sum == 0) return {fixed ? fixed_size : 1, 1, 0, 0, 0, ZinbStatus::Ok};
  const double positive_mean = sum / (n - zeros);
  // Evaluate the exact pi profile. Log-sum-exp prevents zero-mass underflow.
  auto evaluate = [&](double r, double p) {
    const double logf0 = r * std::log(p);
    const double f0 = std::exp(logf0);
    const double pi = f0 < z ? (z - f0) / (-std::expm1(logf0)) : 0;
    double ll = 0;
    for (const auto &kv : counts) {
      double l;
      if (kv.first == 0) {
        const double a = std::log(pi), b = std::log1p(-pi) + logf0;
        const double m = std::max(a, b);
        l = m + std::log1p(std::exp(std::min(a, b) - m));
      } else {
        const double v = kv.first;
        double gamma_difference = special::lgamma(r + v) - special::lgamma(r);
        // log rising factorial expansion avoids subtracting two enormous
        // log-gammas near the Poisson face. Sum powers of 0,...,v-1.
        if (r > 1e4 && v/r < 1e-3) {
          const double k = v-1;
          const double s1 = k*v/2, s2 = k*v*(2*v-1)/6;
          const double s3 = s1*s1;
          const double s4 = k*v*(2*v-1)*(3*k*k+3*k-1)/30;
          gamma_difference = v*std::log(r) + s1/r - s2/(2*r*r)
              + s3/(3*r*r*r) - s4/(4*r*r*r*r);
        }
        l = std::log1p(-pi) + gamma_difference
            - special::lgamma(v + 1) + logf0 + v * std::log1p(-p);
      }
      ll += kv.second * l;
    }
    return ZinbResult{r, p, pi, ll, 0, ZinbStatus::Ok};
  };
  // For fixed r, the truncated NB score in p is zero precisely when its
  // conditional mean equals the empirical positive mean. It decreases
  // continuously from infinity to one as p increases from zero to one.
  auto profile = [&](double r) {
    double lo = 0, hi = 1;
    for (int i = 0; i < 100; ++i) {
      const double p = (lo + hi) / 2;
      const double m = r * (1 - p) / (p * (-std::expm1(r * std::log(p))));
      if (m > positive_mean) lo = p; else hi = p;
    }
    const double p = (lo + hi) / 2;
    // The join belongs to the NB branch. Its optimum has p=r/(r+mean).
    if (r * std::log(p) <= std::log(z)) return evaluate(r, p);
    return evaluate(r, r / (r + sum / n));
  };
  if (fixed) return profile(fixed_size);
  // Include the attained NB member explicitly, reusing its exact fitter.
  const auto nb = nbinom_mle(x, std::min(tol, 1e-10), maxit);
  if (std::isfinite(nb.size) && nb.size > 0 && nb.prob < 1) {
    best = evaluate(nb.size, nb.prob);
  }
  const auto initial = profile(1);
  if (initial.logLik > best.logLik) best = initial;
  const auto zip = zip_mle(x);
  // Expand both ends until the lower end has negligible positive mass and
  // the upper end agrees with the independently maximized infinite-size
  // face. No finite size cap is returned as an approximation to that face.
  int lower = -2, upper = 2;
  while (lower > -700 && profile(std::exp(lower)).logLik > best.logLik - n)
    lower -= 2;
  while (upper < 32 && (zip.status != ZipStatus::Ok ||
         std::abs(profile(std::exp(upper)).logLik - zip.logLik) > 1e-10*n))
    upper += 2;
  if (upper >= 32) {
    best.status = ZinbStatus::IterationFailure;
    return best; // Floating-point resolution exhausted; never return a capped fit.
  }
  const double step = 1;
  for (int j = lower; j <= upper; ++j) {
    const double t = j;
    const auto mid = profile(std::exp(t));
    if (mid.logLik > best.logLik) best = mid;
    if (mid.logLik < profile(std::exp(t-step)).logLik ||
        mid.logLik < profile(std::exp(t+step)).logLik) continue;
    double a = t-step, b = t+step;
    constexpr double ratio = 0.6180339887498948482;
    double c = b-ratio*(b-a), d = a+ratio*(b-a);
    auto fc = profile(std::exp(c)), fd = profile(std::exp(d));
    int i = 0;
    for (; i < maxit && b-a > tol; ++i) {
      if (fc.logLik > fd.logLik) {
        b=d; d=c; fd=fc; c=b-ratio*(b-a); fc=profile(std::exp(c));
      } else {
        a=c; c=d; fc=fd; d=a+ratio*(b-a); fd=profile(std::exp(d));
      }
    }
    auto candidate = fc.logLik > fd.logLik ? fc : fd;
    candidate.iterations = i;
    if (candidate.logLik > best.logLik) best = candidate;
  }
  if (zip.status == ZipStatus::Ok && zip.logLik >= best.logLik - 1e-8*n) {
    best.status = zip.pi == 0 ? ZinbStatus::PoissonLimit : ZinbStatus::ZipLimit;
  }
  return best;
}
} // namespace cppunivariate
