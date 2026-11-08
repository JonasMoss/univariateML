// Direct Euler--Maclaurin tails; no support enumeration or integer narrowing.
#pragma once
#include "mle.hpp"
#include "../zipf/dpqr.hpp"
namespace cppunivariate {
namespace zeta_detail {
inline double log_complement(double lp) {
  return lp < -.6931471805599453 ? std::log1p(-std::exp(lp)) : std::log(-std::expm1(lp));
}
inline bool valid(double s) { return std::isfinite(s) && s>1; }
inline double logtail(double k,double s) {
  const double a=k+1;
  if (a>=64 && s/a<.05) {
    const double r=s/a;
    return (1-s)*std::log(a)+std::log(1/(s-1)+.5/a+
      r/(12*a)-r*((s+1)/a)*((s+2)/a)/(720*a));
  }
  // Sum a fixed prefix scaled by a^-s, then integrate from its last point.
  double sum=0;
  for (int j=0;j<1024;++j) sum+=std::exp(-s*std::log1p(j/a));
  const double b=a+1023, r=s/b;
  const double w=std::exp(-s*std::log1p(1023/a));
  const double tail=b/(s-1)-.5+r/12-r*((s+1)/b)*((s+2)/b)/720;
  // When s/b is large the remainder is already negligible after 1024 terms.
  if (r<.1) sum+=w*tail;
  return -s*std::log(a)+std::log(sum);
}
inline double logbelow(double k,double s) {
  if (k==1) return 0;
  double sum=0;
  for (int j=1;j<=64 && j<=k;++j) sum+=std::exp(-s*std::log(static_cast<double>(j)));
  if (k>64) sum+=std::exp(distributions::zipf_dpqr_detail::log_range(65,k,s));
  return std::log(sum);
}
}
namespace distributions {
inline double dzeta(double x,double s,bool log=false) {
  if (!zeta_detail::valid(s) || std::isnan(x)) return special::kNaN;
  if (!std::isfinite(x) || x<1 || x!=std::floor(x)) return log ? special::kNegInf : 0;
  const double lp=-s*std::log(x)-zeta_detail::logz(s);
  return log ? lp : std::exp(lp);
}
inline double pzeta(double q,double s,bool lower_tail=true,bool log_p=false) {
  if (!zeta_detail::valid(s) || std::isnan(q)) return special::kNaN;
  double lp;
  if (q<1) lp=lower_tail ? special::kNegInf : 0;
  else if (!std::isfinite(q)) lp=lower_tail ? 0 : special::kNegInf;
  else {
    const double k=std::floor(q), den=zeta_detail::logz(s);
    const double above=std::min(0.0,zeta_detail::logtail(k,s)-den);
    if (above<-.6931471805599453)
      lp=lower_tail ? zeta_detail::log_complement(above) : above;
    else {
      const double below=std::min(0.0,zeta_detail::logbelow(k,s)-den);
      lp=lower_tail ? below : zeta_detail::log_complement(below);
    }
  }
  return log_p ? lp : std::exp(lp);
}
inline double qzeta(double p,double s,bool lower_tail=true,bool log_p=false) {
  if (!zeta_detail::valid(s) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return special::kNaN;
  if (p==(log_p ? special::kNegInf : 0)) return lower_tail ? 1 : special::kPosInf;
  if (p==(log_p ? 0 : 1)) return lower_tail ? special::kPosInf : 1;
  return special::discrete_search(1,special::kPosInf,[&](double k) {
    const double f=pzeta(k,s,lower_tail,log_p);
    return std::isnan(f) ? -1 : int(lower_tail ? f>=p : f<=p);
  });
}
template <typename Rng> inline double rzeta(Rng &rng,double s) { return qzeta(rng.runif(0,1),s); }
}
}
