// Yule--Simon beta tail, with stable log gamma increments from beta-NB.
#pragma once
#include "../../core/precision.hpp"
#include "../bnbinom/dpqr.hpp"
namespace cppunivariate {
namespace yulesimon_detail {
inline double log_complement(double lp) {
  return lp < -.6931471805599453 ? std::log1p(-std::exp(lp)) : std::log(-std::expm1(lp));
}
inline bool valid(double rho) { return rho>0 && std::isfinite(rho); }
// log P(X>k) = log Gamma(k+1) Gamma(rho+1)/Gamma(k+rho+1).
inline double logtail(double k,double rho) {
  if (k<=32) {
    CPPUNIV_EXTENDED_REAL out=0;
    for (int j=1;j<=k;++j) out-=std::log1p(static_cast<CPPUNIV_EXTENDED_REAL>(rho)/j);
    return static_cast<double>(out);
  }
  const CPPUNIV_EXTENDED_REAL a=std::min(static_cast<CPPUNIV_EXTENDED_REAL>(k),static_cast<CPPUNIV_EXTENDED_REAL>(rho));
  const CPPUNIV_EXTENDED_REAL b=std::max(static_cast<CPPUNIV_EXTENDED_REAL>(k),static_cast<CPPUNIV_EXTENDED_REAL>(rho));
  // The shared increment can lose a tiny result when equal Stirling
  // corrections are subtracted. For a/b tiny, integrate digamma locally;
  // the omitted cubic term is O(a^3/b^2).
  const CPPUNIV_EXTENDED_REAL increment=a/b<CPPUNIV_EXTENDED_LITERAL(1e-8,1e-8L) ?
    a*special::digamma(static_cast<double>(b+1))+
      CPPUNIV_EXTENDED_LITERAL(.5,.5L)*a*a*special::trigamma(static_cast<double>(b+1)) :
    bnbinom_detail::increment(1+b,a);
  return static_cast<double>((a<=CPPUNIV_EXTENDED_LITERAL(1e-7,1e-7L) ? -CPPUNIV_EXTENDED_LITERAL(0.5772156649015328606,0.5772156649015328606L)*a+CPPUNIV_EXTENDED_LITERAL(0.8224670334241132182,0.8224670334241132182L)*a*a : std::lgamma(1+a))-increment);
}
}
namespace distributions {
inline double dyulesimon(double x,double rho,bool log=false) {
  if (!yulesimon_detail::valid(rho) || std::isnan(x)) return special::kNaN;
  if (!std::isfinite(x) || x<1 || x!=std::floor(x)) return log ? special::kNegInf : 0;
  const double ratio=x/rho;
  const double factor=std::isfinite(ratio) ? -std::log1p(ratio) : std::log(rho)-std::log(x);
  const double lp=yulesimon_detail::logtail(x-1,rho)+factor;
  return log ? lp : std::exp(lp);
}
inline double pyulesimon(double q,double rho,bool lower_tail=true,bool log_p=false) {
  if (!yulesimon_detail::valid(rho) || std::isnan(q)) return special::kNaN;
  double lp;
  if (q<1) lp=lower_tail ? special::kNegInf : 0;
  else if (!std::isfinite(q)) lp=lower_tail ? 0 : special::kNegInf;
  else {
    const double above=yulesimon_detail::logtail(std::floor(q),rho);
    lp=lower_tail ? yulesimon_detail::log_complement(above) : above;
  }
  return log_p ? lp : std::exp(lp);
}
inline double qyulesimon(double p,double rho,bool lower_tail=true,bool log_p=false) {
  if (!yulesimon_detail::valid(rho) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return special::kNaN;
  if (p==(log_p ? special::kNegInf : 0)) return lower_tail ? 1 : special::kPosInf;
  if (p==(log_p ? 0 : 1)) return lower_tail ? special::kPosInf : 1;
  return special::discrete_search(1,special::kPosInf,[&](double k) {
    const double f=pyulesimon(k,rho,lower_tail,log_p);
    return std::isnan(f) ? -1 : int(lower_tail ? f>=p : f<=p);
  });
}
template <typename Rng> inline double ryulesimon(Rng &rng,double rho) { return qyulesimon(rng.runif(0,1),rho); }
}
}
