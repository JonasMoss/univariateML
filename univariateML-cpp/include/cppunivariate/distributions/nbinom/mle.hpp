// Negative Binomial MLE. Newton on size (1D, hybrid-step convergence)
// using the unique-counts trick to avoid recomputing digamma at duplicate
// observations. A finite MLE exists iff the population-form variance
// exceeds the mean (Aragon, Eberly and Eberly 1992); otherwise, including
// equality, the profile likelihood increases to the Poisson limit and the
// result is flagged Underdispersed (caller routes to mlpois). When Newton
// leaves the domain or does not converge, the unique root of the profile
// score is bracketed on a log scale and bisected.

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <map>
#include <vector>

namespace cppunivariate {

enum class NbinomStatus { Ok, Underdispersed, ZeroLimit, InvalidInput, PrecisionFailure };

struct NbinomResult {
  double size;
  double prob;
  double logLik;
  int iterations;
  bool underdispersed;
  NbinomStatus status = NbinomStatus::Ok;
};

inline NbinomResult nbinom_mle(ispan x, double tol, int maxit,
    double size_fixed = std::numeric_limits<double>::quiet_NaN(),
    double prob_fixed = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, false, NbinomStatus::InvalidInput};

  double sum_x = 0.0, sum_x_sq = 0.0;
  for (auto v : x) {
    sum_x += static_cast<double>(v);
    sum_x_sq += static_cast<double>(v) * static_cast<double>(v);
  }
  const double dn = static_cast<double>(n);
  const double x_bar = sum_x / dn;
  if(std::isfinite(size_fixed) && size_fixed>0 && std::isnan(prob_fixed) &&
      sum_x>0 && size_fixed/(size_fixed+x_bar)==1)
    return {kNaN,kNaN,kNaN,0,false,NbinomStatus::PrecisionFailure};
  const auto invalid = NbinomResult{kNaN,kNaN,kNaN,0,false,NbinomStatus::InvalidInput};
  if (!(tol>0) || !std::isfinite(tol) || maxit<1 ||
      (!std::isnan(size_fixed) && (!(size_fixed>=0)||!std::isfinite(size_fixed))) ||
      (!std::isnan(prob_fixed) && (!(prob_fixed>0 && prob_fixed<=1)||!std::isfinite(prob_fixed)))) return invalid;
  for (int v:x) if(v<0) return invalid;
  if (size_fixed==0) return x_bar==0 ? NbinomResult{0,std::isnan(prob_fixed)?.5:prob_fixed,0,0,false} : invalid;
  if (x_bar==0 && std::isnan(size_fixed))
    return {0,std::isnan(prob_fixed)?.5:prob_fixed,0,0,false};
  if (!std::isnan(prob_fixed)) {
    if (prob_fixed==1 && x_bar>0) return invalid;
    double r=size_fixed;
    if (std::isnan(r)) {
      if (prob_fixed==1) r=1;
      else {
        // Strictly decreasing score: unique root, no size-infinity face.
        auto score=[&](double a) {
          double out=dn*std::log(prob_fixed);
          for(int v:x) {
            if(v<64) {for(int j=0;j<v;++j)out+=1/(a+j);}
            else if(a>1e5) {
              const double inv=1/a, end=1/(a+v);
              out+=std::log1p(v/a)+.5*(inv-end)+(inv*inv-end*end)/12;
            } else out+=special::digamma(a+v)-special::digamma(a);
          }
          return out;
        };
        double lo=0,hi=std::max(1.0,x_bar*prob_fixed/(1-prob_fixed));
        while(score(hi)>0 && std::isfinite(hi)) hi*=2;
        if(!std::isfinite(hi)) return invalid;
        for(int i=0;i<200;++i) {
          const double mid=lo+(hi-lo)/2;
          if(score(mid)>0)lo=mid;else hi=mid;
        }
        r=lo+(hi-lo)/2;
      }
    }
    double ll=0;
    for(int v:x) {
      // The generic log-gamma subtraction loses digits at large held-prob
      // size roots. Sum the rising factorial, or its power expansion.
      if(v<10000) {
        double l=r*std::log(prob_fixed)-special::lgamma(v+1.0);
        for(int j=0;j<v;++j)l+=std::log((r+j)*(1-prob_fixed));
        ll+=l;
      } else if(r>1e4 && v/r<1e-3) {
        const double k=v-1.0, vv=v, s1=k*vv/2;
        const double s2=k*vv*(2*vv-1)/6, s3=s1*s1;
        const double s4=k*vv*(2*vv-1)*(3*k*k+3*k-1)/30;
        ll+=r*std::log(prob_fixed)-special::lgamma(vv+1)+vv*std::log(r*(1-prob_fixed))
            +s1/r-s2/(2*r*r)+s3/(3*r*r*r)-s4/(4*r*r*r*r);
      } else ll+=special::dnbinom(v,r,prob_fixed,true);
    }
    return {r,prob_fixed,ll,0,false};
  }
  if (!std::isnan(size_fixed)) {
    if (!(size_fixed > 0) || !std::isfinite(size_fixed))
      return {kNaN, kNaN, kNaN, 0, false};
    const double prob = size_fixed / (size_fixed + x_bar);
    if (prob == 1 && x_bar > 0) return {kNaN, kNaN, kNaN, 0, false};
    double ll = 0;
    for (int v : x) ll += special::dnbinom(v, size_fixed, prob, true);
    return {size_fixed, prob, ll, 0, false};
  }

  double var_hat = sum_x_sq / dn - x_bar * x_bar;
  // The difference of raw moments can lose the entire variance when counts
  // are large. Recompute in centered coordinates before calling the sample
  // underdispersed; retain the original route for ordinary free fits.
  if (var_hat <= x_bar) {
    long double centered = 0.0L;
    for (int v : x) {
      const long double delta = static_cast<long double>(v) - static_cast<long double>(x_bar);
      centered += delta * delta;
    }
    var_hat = static_cast<double>(centered / static_cast<long double>(n));
    // The raw calculation disagreed with the centered calculation. Avoid
    // both a false Poisson diagnosis and a poorly resolved near-limit fit.
    if (var_hat > x_bar)
      return {kNaN, kNaN, kNaN, 0, false, NbinomStatus::PrecisionFailure};
  }
  // At var_hat == x_bar the profile score is negative for all finite sizes
  // too: the supremum is the Poisson limit, not a finite size.
  if (var_hat <= x_bar) {
    return {kNaN, kNaN, kNaN, 0, true, NbinomStatus::Underdispersed};
  }

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
    const double prob = size / (x_bar + size);
    double ll = 0.0;
    for (std::size_t i = 0; i < uniq.size(); ++i) {
      const double xi = static_cast<double>(uniq[i]);
      ll += counts[i] * (cppunivariate::special::lgamma(xi + size) -
                         cppunivariate::special::lgamma(xi + 1.0));
    }
    ll -= dn * cppunivariate::special::lgamma(size);
    ll += dn * size * std::log(prob);
    ll += dn * std::log(1.0 - prob) * x_bar;
    return ll;
  };

  auto f_over_df = [&](double size) {
    const double prob = size / (x_bar + size);
    const double dprob = x_bar / (size * size + x_bar * size);
    double f = 0.0, df = 0.0;
    for (std::size_t i = 0; i < uniq.size(); ++i) {
      const double arg = size + static_cast<double>(uniq[i]);
      f += counts[i] * cppunivariate::special::digamma(arg);
      df += counts[i] * cppunivariate::special::trigamma(arg);
    }
    f -= dn * cppunivariate::special::digamma(size);
    df -= dn * cppunivariate::special::trigamma(size);
    f += dn * std::log(prob);
    df += dn * dprob;
    return f / df;
  };

  // Profile score in size (prob at its maximiser size / (x_bar + size)):
  // positive below the MLE and negative above it.
  auto score = [&](double size) {
    double f = 0.0;
    for (std::size_t i = 0; i < uniq.size(); ++i) {
      f += counts[i] * cppunivariate::special::digamma(size + static_cast<double>(uniq[i]));
    }
    return f - dn * cppunivariate::special::digamma(size) - dn * std::log1p(x_bar / size);
  };

  const double size0 = (x_bar * x_bar) / (var_hat - x_bar);
  double size = size0;
  int iter = 1;
  bool newton_ok = std::isfinite(size) && size > 0.0;
  for (; newton_ok && iter <= maxit; ++iter) {
    const double step = f_over_df(size);
    size -= step;
    if (size <= 0.0 || !std::isfinite(size)) {
      newton_ok = false;
      break;
    }
    if (std::abs(step) < (std::abs(size) * tol + tol)) break;
  }
  if (newton_ok && iter <= maxit) {
    const double prob = size / (x_bar + size);
    return {size, prob, log_likelihood(size), iter, false};
  }

  // Fallback: bracket the root in log(size) and bisect. An unresolved
  // score at the numerical search limit is a precision failure, not proof
  // that the likelihood is maximized at the Poisson limit.
  constexpr double kSizeMax = 1e15;
  double lo = (std::isfinite(size0) && size0 > 0.0) ? std::min(size0, kSizeMax) : 1.0;
  double hi = lo;
  int evals = 0;
  while (score(lo) <= 0.0 && lo > 1e-300) {
    lo *= 0.5;
    ++evals;
  }
  while (score(hi) >= 0.0) {
    if (hi >= kSizeMax * std::max(1.0, x_bar)) {
      return {kNaN, kNaN, kNaN, iter + evals, false, NbinomStatus::PrecisionFailure};
    }
    hi *= 2.0;
    ++evals;
  }
  if (!(score(lo) > 0.0) || !(score(hi) < 0.0)) return {kNaN, kNaN, kNaN, iter + evals, false, NbinomStatus::PrecisionFailure};
  for (int k = 0; k < 200 && hi / lo - 1.0 > 4.0 * std::numeric_limits<double>::epsilon(); ++k) {
    const double mid = std::sqrt(lo) * std::sqrt(hi);
    const double smid = score(mid);
    if (!std::isfinite(smid)) return {kNaN, kNaN, kNaN, iter + evals, false, NbinomStatus::PrecisionFailure};
    if (smid > 0.0) lo = mid; else hi = mid;
    ++evals;
  }
  size = std::sqrt(lo) * std::sqrt(hi);
  const double prob = size / (x_bar + size);
  return {size, prob, log_likelihood(size), iter + evals, false};
}

}  // namespace cppunivariate
