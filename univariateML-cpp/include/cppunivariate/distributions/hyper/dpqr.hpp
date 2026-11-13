// Hypergeometric distribution: scalar d/p/q/r in m (population successes),
// N (total population), k (draws), all whole, with m <= N and k <= N.
// Support max(0, k - (N - m)) <= x <= min(k, m).
// Public arguments follow README (m, N, k); failures n = N - m.

#pragma once
#include "../../core/precision.hpp"
#include "../../special/tails.hpp"

#include "mle.hpp"

#include <algorithm>
#include <cfloat>
#include <cmath>
#include <limits>

namespace cppunivariate {
namespace distributions {

namespace hyper_dpqr_detail {

inline bool valid(double N, double m, double k) {
  return hyper_detail::is_count(N) && hyper_detail::is_count(m) &&
         hyper_detail::is_count(k) && m <= N && k <= N;
}

// P(X <= q) or P(X > q), each summed directly over its own tail.
inline double tail(double q, double N, double m, double k, bool lower_tail) {
  const double lo = std::max(0.0, k - (N - m)), hi = std::min(k, m);
  q = std::floor(q);
  if (q < lo) return lower_tail ? 0.0 : 1.0;
  if (q >= hi) return lower_tail ? 1.0 : 0.0;
  CPPUNIV_EXTENDED_REAL s = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  const double from = lower_tail ? lo : q + 1.0, to = lower_tail ? q : hi;
  for (double x = from; x <= to; x += 1.0) {
    s += std::exp(static_cast<CPPUNIV_EXTENDED_REAL>(hyper_log_pmf(x, N, m, k)));
  }
  return static_cast<double>(std::min(s, CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)));
}

// On short supports, normalize modal recurrence weights instead of adding
// separately rounded log-pmf values. This removes a common normalizer error
// and keeps a near-one log tail on its small-complement scale. Log weights
// also retain tails below the linear underflow range.
inline special::LogTails normalized_tails(double q,double N,double m,double k) {
  const double lo=std::max(0.0,k-(N-m)),hi=std::min(k,m);
  q=std::floor(q);
  if(q<lo) return special::tails_from_log(special::kNegInf,true);
  if(q>=hi) return special::tails_from_log(special::kNegInf,false);
  const double mode=std::max(lo,std::min(hi,std::floor((k+1)*((m+1)/(N+2)))));
  const auto add=[](CPPUNIV_EXTENDED_REAL a,CPPUNIV_EXTENDED_REAL b) {
    if(a==-std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity()) return b;
    if(b==-std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity()) return a;
    return std::max(a,b)+std::log1p(std::exp(-std::abs(a-b)));
  };
  CPPUNIV_EXTENDED_REAL lower=mode<=q?0:-std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity();
  CPPUNIV_EXTENDED_REAL upper=mode>q?0:-std::numeric_limits<CPPUNIV_EXTENDED_REAL>::infinity();
  CPPUNIV_EXTENDED_REAL weight=0;
  for(double x=mode;x>lo;--x) {
    const CPPUNIV_EXTENDED_REAL X=x,M=m,K=k,B=N-m;
    weight+=std::log(X/(M-X+1))+std::log((B-K+X)/(K-X+1));
    if(x-1<=q) lower=add(lower,weight); else upper=add(upper,weight);
  }
  weight=0;
  for(double x=mode;x<hi;++x) {
    const CPPUNIV_EXTENDED_REAL X=x,M=m,K=k,B=N-m;
    weight+=std::log((M-X)/(X+1))+std::log((K-X)/(B-K+X+1));
    if(x+1<=q) lower=add(lower,weight); else upper=add(upper,weight);
  }
  const CPPUNIV_EXTENDED_REAL total=add(lower,upper);
  return special::tails_from_log(static_cast<double>(std::min(lower,upper)-total),lower<upper);
}

}  // namespace hyper_dpqr_detail

inline double dhyper(double x, double m, double N, double k, bool log) {
  if (!hyper_dpqr_detail::valid(N, m, k) || std::isnan(x)) {
    return std::numeric_limits<double>::quiet_NaN();
  }
  const double l = hyper_log_pmf(x, N, m, k);
  return log ? l : std::exp(l);
}

inline double phyper(double q, double m, double N, double k, bool lower_tail,
                     bool log_p) {
  if (!hyper_dpqr_detail::valid(N, m, k) || std::isnan(q)) {
    return std::numeric_limits<double>::quiet_NaN();
  }
  // Bound the recurrence chart's cost; wide supports retain direct summation.
  if(std::min(k,m)-std::max(0.0,k-(N-m))<=4096)
    return special::tail_probability(hyper_dpqr_detail::normalized_tails(q,N,m,k),lower_tail,log_p);
  const double p = hyper_dpqr_detail::tail(q, N, m, k, lower_tail);
  return log_p ? std::log(p) : p;
}

// Smallest x with P(X <= x) >= p (R's discrete-quantile convention).
inline double hyper_sampling_quantile(double p, double m, double N, double k, bool lower_tail,
                     bool log_p) {
  if (!hyper_dpqr_detail::valid(N, m, k) || std::isnan(p)) {
    return std::numeric_limits<double>::quiet_NaN();
  }
  if (log_p) p = std::exp(p);
  if (!(p >= 0.0 && p <= 1.0)) return std::numeric_limits<double>::quiet_NaN();
  if (!lower_tail) p = 1.0 - p;
  const double lo = std::max(0.0, k - (N - m)), hi = std::min(k, m);
  if (p == 0.0) return lo;
  if (p == 1.0) return hi;
  p *= 1.0 - 1000.0 * DBL_EPSILON;
  CPPUNIV_EXTENDED_REAL s = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (double x = lo; x < hi; x += 1.0) {
    s += std::exp(static_cast<CPPUNIV_EXTENDED_REAL>(hyper_log_pmf(x, N, m, k)));
    if (s >= p) return x;
  }
  return hi;
}

inline double qhyper(double p,double m,double N,double k,bool lower_tail,bool log_p) {
  if(!hyper_dpqr_detail::valid(N,m,k)) return special::kNaN;
  const double lo=std::max(0.0,k-(N-m)),hi=std::min(k,m);
  if(std::isnan(p) || (log_p?p>0:p<0 || p>1)) return special::kNaN;
  if(p==(log_p?special::kNegInf:0)) return lower_tail?lo:hi;
  if(p==(log_p?0:1)) return lower_tail?hi:lo;
  return special::discrete_search(lo,hi,[&](double q) {
    const double probability=phyper(q,m,N,k,lower_tail,log_p);
    return special::discrete_reached(probability,p,lower_tail,log_p);
  });
}

template <typename Rng>
inline double rhyper(Rng &rng, double m, double N, double k) {
  return hyper_sampling_quantile(rng.runif(0.0, 1.0), m, N, k, true, false);
}

}  // namespace distributions
}  // namespace cppunivariate
