// Finite Zipf probabilities; bounded-memory harmonic sums and integer inversion.
#pragma once
#include "mle.hpp"
#include "../../special/constants.hpp"
#include <algorithm>
#include <cmath>
#include <vector>
namespace cppunivariate {
namespace distributions {
namespace zipf_dpqr_detail {
inline bool valid(double N, double s) {
  return std::isfinite(N) && N >= 1 && N <= 2147483647.0 &&
    N == std::floor(N) && std::isfinite(s) && s >= 0;
}
// Log sum on [a,b], scaled by a^-s. The first 1024 terms are exact;
// the remainder uses the same Euler--Maclaurin corrections as moments().
inline double log_range(double a, double b, double s) {
  if (a > b) return special::kNegInf;
  if (s == 0) return std::log(b-a+1);
  if (a == 1) {
    if (s<=32) return std::log(zipf_detail::moments(static_cast<int>(b),s).h);
    double z=0;
    for (double k=2;k<=std::min(b,1024.0);++k) z+=std::exp(-s*std::log(k));
    return std::log1p(z);
  }
  const double end = std::min(b,a+1023);
  double total = 0;
  for (double k=a; k<=end; ++k) total += std::exp(-s*std::log1p((k-a)/a));
  if (end < b) {
    const double w = std::exp(-s*std::log1p((end-a)/a));
    // If the omitted part is below double relative precision, avoid large
    // derivative products in the endpoint correction.
    if (w*b > total*1e-17) {
      const double t=std::log1p((b-end)/end), v=1-s;
      total += w*end*(v == 0 ? t : std::expm1(v*t)/v);
      const auto endpoint = [&](double x) {
        const double z=std::exp(-s*std::log1p((x-a)/a));
        return z*(0.5-s/(12*x)+(s/x)*((s+1)/x)*((s+2)/x)/720);
      };
      total += endpoint(b)-endpoint(end);
    }
  }
  return -s*std::log(a)+std::log(total);
}
}
inline double dzipf(double x,double N,double s,bool log=false) {
  if (!zipf_dpqr_detail::valid(N,s) || std::isnan(x)) return special::kNaN;
  if (x<1 || x>N || x!=std::floor(x)) return log ? special::kNegInf : 0;
  if (s==0) return log ? -std::log(N) : 1/N;
  const double lp=-s*std::log(x)-zipf_dpqr_detail::log_range(1,N,s);
  return log ? lp : std::exp(lp);
}
inline double pzipf(double q,double N,double s,bool lower_tail=true,bool log_p=false) {
  if (!zipf_dpqr_detail::valid(N,s) || std::isnan(q)) return special::kNaN;
  const double k=std::floor(q);
  double lp;
  if (k<1) lp=lower_tail ? special::kNegInf : 0;
  else if (k>=N) lp=lower_tail ? 0 : special::kNegInf;
  else if (s==0) {
    const double prob=lower_tail ? k/N : (N-k)/N;
    return log_p ? std::log(prob) : prob;
  } else {
    const double den=zipf_dpqr_detail::log_range(1,N,s);
    // Compute the smaller side directly, then use expm1 for its complement.
    const double below=zipf_dpqr_detail::log_range(1,k,s)-den;
    const double above=zipf_dpqr_detail::log_range(k+1,N,s)-den;
    if (lower_tail) lp=below<above ? below : std::log(-std::expm1(std::min(0.0,above)));
    else lp=above<below ? above : std::log(-std::expm1(std::min(0.0,below)));
  }
  return log_p ? lp : std::exp(lp);
}
inline double qzipf(double p,double N,double s,bool lower_tail=true,bool log_p=false) {
  if (!zipf_dpqr_detail::valid(N,s) || std::isnan(p) ||
      (log_p ? p>0 : (p<0 || p>1))) return special::kNaN;
  if (p==(log_p ? special::kNegInf : 0)) return lower_tail ? 1 : N;
  if (p==(log_p ? 0 : 1)) return lower_tail ? N : 1;
  return special::discrete_search(1,N,[&](double k) {
    const double f=pzipf(k,N,s,lower_tail,log_p);
    return std::isnan(f) ? -1 : int(lower_tail ? f>=p : f<=p);
  });
}
inline std::vector<double> zipf_cdf(int N,double s) {
  std::vector<double> out(static_cast<std::size_t>(N));
  for (int k=1;k<=N;++k) out[static_cast<std::size_t>(k-1)]=pzipf(k,N,s);
  return out;
}
template <typename Rng>
inline int rzipf_from_cdf(Rng &rng,const std::vector<double> &cum) {
  return static_cast<int>(std::lower_bound(cum.begin(),cum.end(),rng.runif(0,1))-cum.begin())+1;
}
template <typename Rng>
inline double rzipf(Rng &rng,double N,double s) { return qzipf(rng.runif(0,1),N,s); }
}
}
