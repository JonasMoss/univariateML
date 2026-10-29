// Stable PIG log masses. The R shell continues to use actuar's dpqr.
#pragma once
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
namespace pig_detail {
inline double logadd(double a, double b) {
  const double m = a > b ? a : b;
  if (!std::isfinite(m)) return m;
  return m + std::log1p(std::exp((a > b ? b : a) - m));
}
// Ratio recurrence avoids both mass underflow and cancellation in p0.
inline std::vector<double> log_masses(int maxk, double mean, double shape) {
  std::vector<double> out(static_cast<std::size_t>(maxk) + 1);
  const double lm = std::log(mean), ls = std::log(shape), l2 = std::log(2.0);
  double la, lb, lr;
  if (std::isinf(mean)) {
    out[0] = -std::sqrt(2.0 * shape);
    la = 0; lb = ls-l2; lr = 0.5*lb;
  } else {
    const double lu = l2 + 2*lm-ls;
    const double lv = logadd(0, lu);
    out[0] = -std::exp(l2+lm-logadd(0, 0.5*lv));
    la = lu-lv; lb = 2*lm-lv; lr = lm-0.5*lv;
  }
  if (maxk > 0) out[1] = out[0]+lr;
  for (int k=2; k<=maxk; ++k) {
    const double dk = static_cast<double>(k);
    lr = logadd(la+std::log1p(-1.5/dk), lb-std::log(dk)-std::log(dk-1)-lr);
    out[k] = out[k-1]+lr;
  }
  return out;
}
}
namespace distributions {
inline double dpig(double x, double mean, double shape, bool log) {
  if (!(mean > 0 && shape > 0) || !std::isfinite(shape) || std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  if (!std::isfinite(x) || x < 0 || x != std::floor(x))
    return log ? -std::numeric_limits<double>::infinity() : 0;
  // Counts beyond the loop's integer range are unsupported.
  if (x >= std::numeric_limits<int>::max()) return std::numeric_limits<double>::quiet_NaN();
  const double v = pig_detail::log_masses(static_cast<int>(x), mean, shape).back();
  return log ? v : std::exp(v);
}
}
}

#include "../../special/special.hpp"
#include "../invgauss/dpqr.hpp"
namespace cppunivariate { namespace pig_detail {
// Streaming version of log_masses: O(1) storage for CDFs and inversion.
struct MassStream {
 double la,lb,lr,lmass;
 int k=0;
 MassStream(double mean,double shape) {
  const double lm=std::log(mean),ls=std::log(shape);
  const double lu=std::log(2.0)+2*lm-ls,lv=logadd(0,lu);
  lmass=-std::exp(std::log(2.0)+lm-logadd(0,.5*lv));
  la=lu-lv;lb=2*lm-lv;lr=lm-.5*lv;
 }
 void next() {
  ++k;
  if(k>1) lr=logadd(la+std::log1p(-1.5/k),lb-std::log(double(k))-std::log(double(k-1))-lr);
  lmass+=lr;
 }
 // For j>=k+1, r_j <= A+B/((k+1)*k*A*(1-1.5/k)).
 // This follows from r_k>=A*(1-1.5/k), and decreases with k.
 double remainder_bound() const {
  if(k<2) return std::numeric_limits<double>::infinity();
  const double lrmax=logadd(la,lb-std::log(double(k+1))-std::log(double(k))-la-std::log1p(-1.5/k));
  if(lrmax>=0) return std::numeric_limits<double>::infinity();
  return lmass+lrmax-cppunivariate::special::fs_log1mexp(lrmax);
 }
};
constexpr int sum_limit=10000000;
inline double log_lower(int n,double mean,double shape) {
 MassStream s(mean,shape);
 double total=s.lmass;
 while(s.k<n) {s.next();total=logadd(total,s.lmass);}
 return total;
}
inline double log_upper(int n,double mean,double shape) {
 MassStream s(mean,shape);
 while(s.k<=n) s.next();
 double total=s.lmass;
 while(s.k<sum_limit) {
  if(s.remainder_bound()<total+std::log(2e-16)) return total;
  s.next();total=logadd(total,s.lmass);
 }
 // Do not silently return a truncated, uncertified tail.
 return std::numeric_limits<double>::quiet_NaN();
}
} namespace distributions {
inline double ppig(double q,double mean,double shape,bool lower_tail,bool log_p) {
 if(!(mean>0 && shape>0) || !std::isfinite(mean) || !std::isfinite(shape) || std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 const double inf=std::numeric_limits<double>::infinity();
 double lp;
 if(q<0) lp=lower_tail ? -inf : 0;
 else if(q==inf) lp=lower_tail ? 0 : -inf;
 else {
  if(q>=pig_detail::sum_limit) return std::numeric_limits<double>::quiet_NaN();
  const int n=static_cast<int>(std::floor(q));
  const double ll=pig_detail::log_lower(n,mean,shape);
  if(ll<-.6931471805599453) lp=lower_tail ? ll : cppunivariate::special::fs_log1mexp(ll);
  else {
   const double lu=pig_detail::log_upper(n,mean,shape);
   lp=lower_tail ? cppunivariate::special::fs_log1mexp(lu) : lu;
  }
 }
 return log_p ? lp : std::exp(lp);
}
inline double qpig(double p,double mean,double shape,bool lower_tail,bool log_p) {
 if(!(mean>0 && shape>0) || !std::isfinite(mean) || !std::isfinite(shape) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 double lp=log_p ? p : std::log(p);
 if(lp>-.6931471805599453) {lp=cppunivariate::special::fs_log1mexp(lp);lower_tail=!lower_tail;}
 if(lp==-std::numeric_limits<double>::infinity()) return lower_tail ? 0 : std::numeric_limits<double>::infinity();
 auto reached=[&](int n) {const double v=ppig(n,mean,shape,lower_tail,true);return lower_tail ? v>=lp : v<=lp;};
 if(reached(0)) return 0;
 int lo=0,hi=1;
 while(!reached(hi)) {
  lo=hi;
  if(hi>=(pig_detail::sum_limit-1)/2) return std::numeric_limits<double>::quiet_NaN();
  hi*=2;
 }
 while(hi-lo>1) {const int mid=lo+(hi-lo)/2;if(reached(mid)) hi=mid;else lo=mid;}
 return hi;
}
template <typename Rng> inline double rpig(Rng &rng,double mean,double shape) {
 if(!(mean>0 && shape>0) || !std::isfinite(mean) || !std::isfinite(shape)) return std::numeric_limits<double>::quiet_NaN();
 return rng.rpois(rinvgauss(rng,mean,shape));
}
} }
