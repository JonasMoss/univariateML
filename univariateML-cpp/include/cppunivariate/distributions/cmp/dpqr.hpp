#pragma once
#include "../../core/precision.hpp"
#include "../../special/cmp.hpp"
#include "../../special/tails.hpp"
namespace cppunivariate {
namespace cmp_dpqr_detail {
inline bool valid(double lambda,double nu) {
 return std::isfinite(lambda) && lambda>=0 && std::isfinite(nu) && nu>=0 && (nu>0 || lambda<1);
}
inline double log_complement(double x) {
 return x < -0.6931471805599453 ? std::log1p(-std::exp(x)) : std::log(-std::expm1(x));
}
struct Law {
 double lambda,nu,eta;
 CmpMoments moments;
 Law(double l,double n):lambda(l),nu(n),eta(std::log(l)) {
  if (l>0 && n>0) moments=cmp_moments(eta,n);
 }
 double density(double x,bool log) const {
  if (x<0 || !std::isfinite(x) || x!=std::floor(x)) return log ? special::kNegInf : 0;
  CPPUNIV_EXTENDED_REAL lp;
  if (lambda==0) lp=x==0 ? 0 : special::kNegInf;
  else if (nu==0) lp=x*static_cast<CPPUNIV_EXTENDED_REAL>(eta)+std::log1p(-lambda);
  else {
   if (moments.status!=CmpStatus::Ok) return special::kNaN;
   lp=(x-moments.center)*(static_cast<CPPUNIV_EXTENDED_REAL>(eta)-nu*moments.slope)-
      nu*cmp_detail::residual(x,moments.center,moments.slope)-moments.log_scaled_z;
  }
  return log ? static_cast<double>(lp) : static_cast<double>(std::exp(lp));
 }
 double cdf(double q,bool lower,bool log) const {
  q=std::floor(q);
  if (q<0 || q==special::kPosInf || lambda==0) {
   const bool one=lower ? q>=0 : q<0;
   return log ? (one ? 0 : special::kNegInf) : (one ? 1 : 0);
  }
  if (nu==0) {
   const double upper=static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(q)+1)*eta);
   const double lp=lower ? log_complement(upper) : upper;
   return log ? lp : std::exp(lp);
  }
  if (moments.status!=CmpStatus::Ok) return special::kNaN;
  const bool down=q<moments.center;
  CPPUNIV_EXTENDED_REAL k=down ? q : q+1, term=1, sum=1;
  const double first=density(static_cast<double>(k),true);
  bool done=false;
  for (int i=0;i<1000000;++i) {
   if (down && k==0) {done=true; break;}
   const CPPUNIV_EXTENDED_REAL lr=down ? nu*std::log(k)-eta : eta-nu*std::log(k+1);
   const CPPUNIV_EXTENDED_REAL ratio=std::exp(lr);
   if (ratio<1 && term*ratio/(-std::expm1(lr))<=CPPUNIV_EXTENDED_LITERAL(1e-18,1e-18L)*sum) {done=true; break;}
   term*=ratio; sum+=term; k+=down ? -1 : 1;
  }
  if (!done) return special::kNaN;
  const double small=std::min(0.0,first+static_cast<double>(std::log(sum)));
  const double lp=lower==down ? small : log_complement(small);
  return log ? lp : std::exp(lp);
 }
 double quantile(double p,bool lower,bool log) const {
  if (std::isnan(p) || (log ? p>0 : p<0 || p>1)) return special::kNaN;
  if (lambda==0) return 0;
  if (p==(log ? special::kNegInf : 0)) return lower ? 0 : special::kPosInf;
  if (p==(log ? 0 : 1)) return lower ? special::kPosInf : 0;
  const double target=log ? p : std::log(p);
  return special::discrete_search(0,special::kPosInf,[&](double k) {
   if (k>9007199254740992.0) {
     const double f=cdf(k,lower,log);
     return std::isnan(f) ? -1 : int(lower ? f>=p : f<=p);
   }
   const double f=cdf(k,lower,true);
   return std::isnan(f) ? -1 : int(lower ? f>=target : f<=target);
  });
 }
};
}
namespace distributions {
inline double dcmp(double x,double lambda,double nu,bool log=false) {
 if (!cmp_dpqr_detail::valid(lambda,nu) || std::isnan(x)) return special::kNaN;
 return cmp_dpqr_detail::Law(lambda,nu).density(x,log);
}
inline double pcmp(double q,double lambda,double nu,bool lower_tail=true,bool log_p=false) {
 if (!cmp_dpqr_detail::valid(lambda,nu) || std::isnan(q)) return special::kNaN;
 return cmp_dpqr_detail::Law(lambda,nu).cdf(q,lower_tail,log_p);
}
inline double qcmp(double p,double lambda,double nu,bool lower_tail=true,bool log_p=false) {
 if (!cmp_dpqr_detail::valid(lambda,nu)) return special::kNaN;
 return cmp_dpqr_detail::Law(lambda,nu).quantile(p,lower_tail,log_p);
}
template <typename Rng>
inline double rcmp(Rng &rng,double lambda,double nu) {return qcmp(rng.runif(0,1),lambda,nu);}
}
}
