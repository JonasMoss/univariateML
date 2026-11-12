// Stable PIG log masses and tail probabilities.
#pragma once
#include <cmath>
#include <limits>
#include <vector>
#include "../../special/special.hpp"
namespace cppunivariate {
namespace pig_detail {
// Large-order K expansion: E[exp(-c/T)], T~Gamma(x-1/2).
// Taylor remainder is bounded by the next absolute term, provided j<x-1/2.
inline double asympt_series(double x,double c) {
  double term=1,total=1;
  for(int j=1;j<200;++j) {
    term*=-(c/(x-.5-j))/j;
    total+=term;
    if(std::abs(term)<2e-16*std::abs(total)) break;
  }
  return total;
}
inline double log_a(double mean,double shape) {
  return -special::log1p_product(shape/mean/2,1/mean);
}
inline bool use_asympt(double x,double mean,double shape) {
  const double c=shape*.5+(shape/mean)*(shape/mean)*.25;
  return x>100000 && c<x*.25;
}
inline double asympt_mass(double x,double mean,double shape) {
  const double la=log_a(mean,shape), c=shape*.5+(shape/mean)*(shape/mean)*.25;
  return .5*(std::log(shape)-std::log(2*3.14159265358979323846))+shape/mean-.5*la
    +x*la+special::log_gamma_ratio(x,-.5,1)+std::log(asympt_series(x,c));
}
inline double asympt_upper(double n,double mean,double shape) {
  const double la=log_a(mean,shape), a=std::exp(la);
  const double c=shape*.5+(shape/mean)*(shape/mean)*.25;
  // When count spacing vanishes, the tail is geometric to relative
  // O(1/(n*(1-A))); use it only below rounding error.
  if(n*(-std::expm1(la))>1e16)
    return asympt_mass(n,mean,shape)-special::log1mexp(la);
  const double C=mean/std::sqrt(1+2*mean*mean/shape);
  return special::log_series(asympt_mass(n,mean,shape),n,
    [&](double k) {return a*((k-.5)/(k+1))*asympt_series(k+1,c)/asympt_series(k,c);},
    [&](double k) {return a+std::max(C-1.5*a,0.0)/(k+1);});
}
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
  if (!(mean > 0 && shape >= 0) || !std::isfinite(shape) || std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  if (!std::isfinite(x) || x < 0 || x != std::floor(x))
    return log ? -std::numeric_limits<double>::infinity() : 0;
  // As shape tends to zero, all probability concentrates at zero.
  if (shape == 0) return x == 0 ? (log ? 0 : 1) :
      (log ? -std::numeric_limits<double>::infinity() : 0);
  if(pig_detail::use_asympt(x,mean,shape)) {
    const double v=pig_detail::asympt_mass(x,mean,shape);
    return log ? v : std::exp(v);
  }
  // The recurrence covers the small-order region.
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
 if(!(mean>0 && shape>=0) || !std::isfinite(mean) || !std::isfinite(shape) || std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 const double inf=std::numeric_limits<double>::infinity();
 double lp;
 if(q<0) lp=lower_tail ? -inf : 0;
 else if(q==inf || shape==0) lp=lower_tail ? 0 : -inf;
 else {
  if(pig_detail::use_asympt(std::floor(q)+1,mean,shape))
    return special::tail_probability(special::tails_from_log(pig_detail::asympt_upper(std::floor(q)+1,mean,shape),false),lower_tail,log_p);
  if(q>=pig_detail::sum_limit) return std::numeric_limits<double>::quiet_NaN();
  const int n=static_cast<int>(std::floor(q));
  if(n==0) return special::tail_probability(
      special::tails_from_log(pig_detail::MassStream(mean,shape).lmass,true),lower_tail,log_p);
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
 if(!(mean>0 && shape>=0) || !std::isfinite(mean) || !std::isfinite(shape) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),0,shape==0?0:special::kPosInf,
   [&](double x) {
     const double lower=ppig(x,mean,shape,true,true);
     return lower<-.6931471805599453 ? special::tails_from_log(lower,true)
       : special::tails_from_log(ppig(x,mean,shape,false,true),false);
   },p,lower_tail,log_p);
}
template <typename Rng> inline double rpig(Rng &rng,double mean,double shape) {
 if(!(mean>0 && shape>=0) || !std::isfinite(mean) || !std::isfinite(shape)) return std::numeric_limits<double>::quiet_NaN();
 if(shape==0) return 0;
 return rng.rpois(rinvgauss(rng,mean,shape));
}
} }
