// Binomial MLE.
//
// If `size_or_nan` is finite, treat it as a user-supplied total-trials
// value: size is fixed and prob is the closed-form mean(x)/size; the
// caller's responsibility to validate size >= max(x).
//
// Otherwise (size_or_nan is NaN) auto-fit with a bracketed integer profile
// search. Status
// enum surfaces SizeTooSmall, Underdispersed, IterationFailure.

#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "dpqr.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <map>
#include <vector>

namespace cppunivariate {

enum class BinomStatus {
  Ok = 0,
  SizeTooSmall,
  Underdispersed,
  IterationFailure,
  InvalidInput
};

struct BinomResult {
  double size;
  double prob;
  double logLik;
  int iterations;
  BinomStatus status;
  StopDetail stop_detail{};
};

inline double binom_lchoose(double n, double k) {
  return cppunivariate::special::lgamma(n + 1.0) - cppunivariate::special::lgamma(k + 1.0) -
         cppunivariate::special::lgamma(n - k + 1.0);
}

inline BinomResult binom_mle(ispan x, double size_or_nan, double tol,
                             int maxit, double held_prob = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto stopped = [&]() -> BinomResult {
  (void)tol;
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, BinomStatus::IterationFailure};

  for (int v : x) if (v < 0)
    return {kNaN,kNaN,kNaN,0,BinomStatus::InvalidInput};
  if (!std::isnan(size_or_nan) &&
      !(std::isfinite(size_or_nan) && size_or_nan >= 0 && size_or_nan == std::floor(size_or_nan)))
    return {kNaN,kNaN,kNaN,0,BinomStatus::InvalidInput};

  double sum_x = 0.0, sum_x_sq = 0.0;
  int max_x = x[0];
  for (auto v : x) {
    sum_x += static_cast<double>(v);
    sum_x_sq += static_cast<double>(v) * static_cast<double>(v);
    if (v > max_x) max_x = v;
  }
  const double dn = static_cast<double>(n);
  const double x_bar = sum_x / dn;
  // Two-pass (deviations-from-mean) form, not sum_x_sq/dn - x_bar*x_bar:
  // the naive one-pass formula cancels catastrophically whenever the
  // dispersion gap x_bar - var_pop is small relative to x_bar*x_bar,
  // which is exactly the regime that produces a large free-size MLE
  // (see the "huge optimal size" test below). Getting the gap's sign
  // right is what the Underdispersed escape depends on.
  double var_pop = 0.0;
  for (auto v : x) {
    const double d = static_cast<double>(v) - x_bar;
    var_pop += d * d;
  }
  var_pop /= dn;

  if (!std::isnan(held_prob)) {
    const auto bad = [&]() { return BinomResult{kNaN,kNaN,kNaN,0,BinomStatus::InvalidInput}; };
    if (!(held_prob>=0 && held_prob<=1) || !std::isfinite(held_prob) || maxit<1 || !(tol>0)) return bad();
    for (int v:x) if (v<0) return bad();
    double size=size_or_nan;
    int iterations=0;
    if (!std::isnan(size) && (!std::isfinite(size) || size<max_x || size!=std::floor(size))) return bad();
    if (std::isnan(size)) {
      if (held_prob==0) { if (max_x>0) return bad(); size=0; }
      else if (held_prob==1) { if (var_pop!=0) return bad(); size=max_x; }
      else {
        // Adjacent log likelihood ratio decreases strictly in integer size.
        const auto delta=[&](double k) {
          double d=dn*std::log1p(-held_prob);
          for (int v:x) d-=std::log1p(-v/(k+1));
          return d;
        };
        double lo=max_x,hi=std::max(1.,lo);
        while (delta(hi)>0) {
          if (++iterations>=maxit || hi>=4503599627370496.) return BinomResult{kNaN,kNaN,kNaN,iterations,BinomStatus::IterationFailure};
          hi*=2;
        }
        while (lo<hi) {
          if (++iterations>=maxit) return BinomResult{kNaN,kNaN,kNaN,iterations,BinomStatus::IterationFailure};
          const double mid=std::floor(lo+(hi-lo)/2);
          if (delta(mid)>0) lo=mid+1; else hi=mid;
        }
        size=lo;
      }
    }
    double ll=0;
    for (int v:x) {
      if ((held_prob==0 && v!=0) || (held_prob==1 && v!=size)) return bad();
      ll+=binom_lchoose(size,v);
      if (v>0) ll+=v*std::log(held_prob);
      if (size>v) ll+=(size-v)*std::log1p(-held_prob);
    }
    return {size,held_prob,ll,iterations,BinomStatus::Ok};
  }

  // Build sorted unique values + counts (replaces arma::unique + match).
  std::map<int, int> count_map;
  for (auto v : x) ++count_map[v];
  std::vector<int> uniq;
  std::vector<int> counts;
  uniq.reserve(count_map.size());
  counts.reserve(count_map.size());
  for (const auto &kv : count_map) {
    uniq.push_back(kv.first);
    counts.push_back(kv.second);
  }

  auto log_likelihood = [&](double size) {
    // Continuous extension x*log(x) = 0 at x = 0.
    if (x_bar == 0.0 || size == x_bar) return 0.0;
    if (static_cast<double>(max_x) / size < 1e-5) {
      // Poisson-scale form of the exact binomial likelihood. Expand only
      // sum_{j=0}^{x-1} log(1-j/size), with error O(x (x/size)^4).
      double ll = 0.0;
      for (std::size_t i = 0; i < uniq.size(); ++i) {
        const double k = static_cast<double>(uniq[i]);
        const double q = (k - 1.0) / size;
        const double correction = -0.5 * k * q -
            k * q * ((2.0 * k - 1.0) / size) / 12.0 -
            k * k / size * q * q / 12.0;
        ll += counts[i] * (k * std::log(x_bar) - std::lgamma(k + 1.0) +
                          (size - k) * std::log1p(-x_bar / size) + correction);
      }
      return ll;
    }
    // On double-width long double, the lgamma differences below can lose
    // many digits. For moderate counts sum the exact falling-factorial
    // correction after combining k*log(size) with k*log(p).
    if (std::numeric_limits<CPPUNIV_EXTENDED_REAL>::digits <=
            std::numeric_limits<double>::digits && max_x <= 100000) {
      double ll = 0.0;
      const double p = x_bar / size;
      std::vector<double> correction(static_cast<std::size_t>(max_x) + 1, 0.0);
      for (int j = 1; j <= max_x; ++j)
        correction[static_cast<std::size_t>(j)] = correction[static_cast<std::size_t>(j - 1)] +
            std::log1p(-(j - 1) / size);
      for (std::size_t i = 0; i < uniq.size(); ++i) {
        const int k = uniq[i];
        ll += counts[i] * (k * std::log(x_bar) - std::lgamma(k + 1.0) +
                           correction[static_cast<std::size_t>(k)] +
                           (size - k) * std::log1p(-p));
      }
      return ll;
    }
    // Long double throughout: this branch runs whenever max_x/size >=
    // 1e-5, i.e. size/max_x <= 1e5, and CPPUNIV_EXTENDED_REAL's ~19-digit lgamma
    // difference loses only about log10(size/max_x) <~ 5 digits there --
    // negligible. (Verified against an mpmath oracle: matches to >=10
    // significant digits at size/max_x up to 1e5; the double-precision
    // form this replaced was off by ~1e-3 in logLik at size ~3e7,
    // max_x ~1e4 -- exactly the regime the free-size search above now
    // reaches once its cap is gone.) Two further stability points:
    // - the "entropy" term uses p = x_bar/size directly, not
    //   (size - x_bar) via subtraction -- the same cancellation this
    //   file's `increment` comment above explains for the search itself
    //   applies verbatim to the old `size_m_x * log(size_m_x) -
    //   size * log(size)` form;
    // - lchoose uses std::lgammal (CPPUNIV_EXTENDED_REAL), matching the precedent
    //   of calling std:: math functions directly already used in the
    //   Poisson-scale branch above, not cppunivariate::special::lgamma
    //   (double-only, and out of scope to change here).
    const CPPUNIV_EXTENDED_REAL sizeL = size, x_barL = x_bar, pL = x_barL / sizeL;
    CPPUNIV_EXTENDED_REAL val = static_cast<CPPUNIV_EXTENDED_REAL>(dn) * x_barL * std::log(pL) +
                       static_cast<CPPUNIV_EXTENDED_REAL>(dn) * (sizeL - x_barL) * std::log1p(-pL);
    for (std::size_t i = 0; i < uniq.size(); ++i) {
      const CPPUNIV_EXTENDED_REAL k = static_cast<CPPUNIV_EXTENDED_REAL>(uniq[i]);
      val += static_cast<CPPUNIV_EXTENDED_REAL>(counts[i]) *
             (std::lgamma(sizeL + CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)) - std::lgamma(k + CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)) - std::lgamma(sizeL - k + CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)));
    }
    return static_cast<double>(val);
  };

  // User-supplied size.
  if (std::isfinite(size_or_nan)) {
    if (size_or_nan < static_cast<double>(max_x)) {
      return {kNaN, kNaN, kNaN, 0, BinomStatus::SizeTooSmall};
    }
    const double prob = size_or_nan == 0.0 ? 0.0 : x_bar / size_or_nan;
    return {size_or_nan, prob, log_likelihood(size_or_nan), 0,
            BinomStatus::Ok};
  }

  // All-zero data: size is not identifiable; choose size=0, prob=0.
  if (x_bar == 0.0) return {0.0, 0.0, 0.0, 0, BinomStatus::Ok};

  if (uniq.size() == 1) return {static_cast<double>(max_x),1.0,0.0,0,BinomStatus::Ok};

  if (x_bar / var_pop <= 1.0) {
    return {kNaN, kNaN, kNaN, 0, BinomStatus::Underdispersed};
  }

  // DeRiggi (1983) proves the integer profile likelihood in size, after
  // profiling out prob at its own conditional MLE p = mean(x)/size, is
  // unimodal: its forward difference ("increment" below) changes sign at
  // most once, from positive to non-positive, whenever a finite MLE
  // exists (the x_bar/var_pop > 1 case established above; the <= 1 case
  // is the Poisson-limit escape already returned). So bracketing that
  // single sign change and bisecting it is a certificate, not a
  // heuristic: the maximizer is the smallest integer size for which the
  // increment is <= 0, and there is exactly one such crossing to find,
  // however far out it lies.
  //
  // There is therefore no sample for which this search should be capped
  // at a fixed magnitude; the only legitimate stopping conditions are (a)
  // finding the crossing, or (b) exhausting the iteration budget `maxit`
  // (which the caller can raise without bound; doubling needs only
  // O(log2(size)) iterations, so any size reachable in double/CPPUNIV_EXTENDED_REAL
  // arithmetic is reachable in a modest iteration count). The bracket is
  // kept in `CPPUNIV_EXTENDED_REAL` rather than an integer type: doubling a signed
  // 64-bit integer without a cap can overflow (undefined behaviour) long
  // before any real sample needs a size that large, whereas `CPPUNIV_EXTENDED_REAL`
  // (64-bit mantissa on the x86-64 extended format this project targets)
  // represents every integer up to 2^64 exactly and degrades gracefully,
  // not silently, beyond that.
  //
  // The naive log1p rearrangement of the "entropy" part of the increment
  // (literally -m*log1p(1/z)+(z+1-m)*log1p(-m/(z+1))-(z-m)*log1p(-m/z))
  // cancels catastrophically once z grows past roughly max_x/eps: each of
  // its three terms is O(z) while the sum is the O(1/z^2) increment
  // itself, so CPPUNIV_EXTENDED_REAL's ~19 digits run out by z ~ 1e7-1e8 for
  // typical data (verified against an independent mpmath oracle at
  // dps=60: for a sample engineered to have its true maximizer near
  // 3.3e7, the naive form's sign flips 5e6 early, at 2.8e7). The fix uses
  // the exact identity g(y+1)-g(y) = log(y+1) + y*log1p(1/y) for
  // g(w)=w*log(w) (applied to y=z-m and y=z) to remove the first level of
  // cancellation, and then phi(w) := w*log1p(1/w) - 1 = sum_{j>=1}
  // (-1)^j/((j+1)*w^j) -- a directly convergent series, no subtraction
  // of near-equal O(1) numbers -- to remove the second level. Both
  // halves are then differences of independently-accurate small
  // quantities, not of large near-equal ones, and the formula tracks the
  // mpmath oracle to its last representable CPPUNIV_EXTENDED_REAL digit at every
  // scale tested, including sizes beyond 1e12 (see test.cpp/test.R).
  auto phi = [](CPPUNIV_EXTENDED_REAL w) {
    if (w < CPPUNIV_EXTENDED_LITERAL(1000.0,1000.0L)) return w*std::log1p(CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)/w) - CPPUNIV_EXTENDED_LITERAL(1.0,1.0L);
    CPPUNIV_EXTENDED_REAL term = -CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)/(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)*w), sum = term;
    for (int j = 1; j <= 40; ++j) {
      term *= -(static_cast<CPPUNIV_EXTENDED_REAL>(j+1)/(j+2))/w;
      sum += term;
      if (std::fabs(term) < std::fabs(sum)*CPPUNIV_EXTENDED_LITERAL(1e-25,1e-25L)) break;
    }
    return sum;
  };
  auto increment = [&](CPPUNIV_EXTENDED_REAL k) {
    const CPPUNIV_EXTENDED_REAL z=k, m=static_cast<CPPUNIV_EXTENDED_REAL>(sum_x)/n;
    CPPUNIV_EXTENDED_REAL d=0;
    for (std::size_t i=0;i<uniq.size();++i)
      d+=static_cast<CPPUNIV_EXTENDED_REAL>(counts[i])*std::log1p(
        static_cast<CPPUNIV_EXTENDED_REAL>(uniq[i])/(z+1-uniq[i]));
    d+=static_cast<CPPUNIV_EXTENDED_REAL>(n)*(std::log1p(-m/(z+1)) + phi(z-m) - phi(z));
    return d;
  };
  CPPUNIV_EXTENDED_REAL lo=max_x, hi=lo;
  int iterations=0;
  while(increment(hi)>0) {
    if (++iterations>=maxit || !std::isfinite(hi)) return {kNaN,kNaN,kNaN,iterations,BinomStatus::IterationFailure};
    lo=hi+1;
    hi=hi*2;
  }
  while(hi>lo) {
    if (++iterations>=maxit) return {kNaN,kNaN,kNaN,iterations,BinomStatus::IterationFailure};
    CPPUNIV_EXTENDED_REAL mid=lo+std::floor((hi-lo)/CPPUNIV_EXTENDED_LITERAL(2.0,2.0L));
    if (mid<lo) mid=lo;           // resolution guard at extreme magnitudes
    if (mid>=hi) mid=hi-1;        // guarantees progress every iteration
    if(increment(mid)>0) lo=mid+1; else hi=mid;
  }
  const double best_size=static_cast<double>(hi);
  return {best_size,x_bar/best_size,log_likelihood(best_size),iterations,BinomStatus::Ok};

  }();
  if(stopped.status==BinomStatus::Underdispersed) {
    double m=0;for(int v:x)m+=v;m/=x.size();double ll=0;
    for(int v:x)ll+=v*std::log(m)-m-special::lgamma(v+1.);
    stopped.stop_detail=discrete_face(StopModel::Pois,{m},ll);
  }
  if(stopped.status==BinomStatus::Ok && !std::isnan(held_prob)) {
    double ll=0,correction=0;
    for(int v:x) {
      const double value=distributions::dbinom(v,stopped.size,stopped.prob,true)-correction;
      const double next=ll+value;correction=(next-ll)-value;ll=next;
    }
    stopped.logLik=ll;
    if(!std::isfinite(ll))stopped.status=BinomStatus::IterationFailure;
  }
  return stopped;
}

}  // namespace cppunivariate
