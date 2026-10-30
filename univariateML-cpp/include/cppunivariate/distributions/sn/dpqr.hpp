#pragma once
#include "../../core/precision.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
namespace cppunivariate {
namespace sn_detail {
inline bool valid(double xi,double omega,double alpha) {
  return std::isfinite(xi) && std::isfinite(omega) && omega>0 && std::isfinite(alpha);
}
inline double log_complement(double lp) {
  return lp < -std::log(2.0) ? std::log1p(-std::exp(lp)) : std::log(-std::expm1(lp));
}
inline double log_phi_cdf(double t) {
  if(t>0) return std::log1p(-0.5*std::erfc(t/std::sqrt(2.0)));
  if (t>=-10) return std::log(0.5*std::erfc(-t/std::sqrt(2.0)));
  const double v=-t;
  double r=0;
  for (int k=24;k>=1;--k) r=k/(v+r);
  return -0.5*t*t-0.5*std::log(2*M_PI)-std::log(v+r);
}
inline double inverse_mills(double t) {
  if(t < -10) {
    double f=0;
    for(int k=24;k>=1;--k) f=k/(-t+f);
    return -t+f;
  }
  return std::exp(-0.5*t*t-0.5*std::log(2*M_PI)-log_phi_cdf(t));
}
inline double log_density(double z,double a) {
  return std::log(2.0)-0.5*std::log(2*M_PI)-0.5*z*z+log_phi_cdf(a*z);
}
// Cancellation-free integral of the decreasing density away from a boundary.
// Used on the thin side, and for normal tails beyond double CDF range.
inline double log_integral(double z,double a,bool left) {
  const double az=a*z;
  const double mills=az < -10 ? -az+1/(-az) :
    std::exp(-0.5*az*az-0.5*std::log(2*M_PI)-log_phi_cdf(az));
  const CPPUNIV_EXTENDED_REAL slope=-static_cast<CPPUNIV_EXTENDED_REAL>(z)+static_cast<CPPUNIV_EXTENDED_REAL>(a)*mills;
  const double scale=static_cast<double>(1/std::max(CPPUNIV_EXTENDED_LITERAL(1.0,1.0L),std::abs(slope)));
  const double base=log_density(z,a);
  if (!std::isfinite(base)) return base;
  const double nodes[4]={0.1834346424956498,0.5255324099163290,0.7966664774136267,0.9602898564975363};
  const double weights[4]={0.3626837833783620,0.3137066458778873,0.2223810344533745,0.1012285362903763};
  double sum=0;
  for (int i=0;i<80;++i) for(int j=0;j<4;++j) for(double sign:{-1.0,1.0}) {
    const double u=(i+0.5+sign*nodes[j]/2)*0.5;
    const double t=z+(left ? -1 : 1)*scale*u;
    sum+=weights[j]*0.25*std::exp(log_density(t,a)-base);
  }
  return base+std::log(scale)+std::log(sum);
}
}
namespace distributions {
inline double dsn(double x,double xi=0,double omega=1,double alpha=0,bool log=false) {
  if (!sn_detail::valid(xi,omega,alpha) || std::isnan(x)) return special::kNaN;
  if (!std::isfinite(x)) return log ? special::kNegInf : 0;
  const double z=static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(x)-xi)/omega);
  if(!std::isfinite(z)) return log ? special::kNegInf : 0;
  const double lp=sn_detail::log_density(z,alpha)-std::log(omega);
  return log ? lp : std::exp(lp);
}
inline double psn(double q,double xi=0,double omega=1,double alpha=0,bool lower_tail=true,bool log_p=false) {
  if (!sn_detail::valid(xi,omega,alpha) || std::isnan(q)) return special::kNaN;
  double z=static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(q)-xi)/omega),a=alpha;
  if (a<0) { a=-a; z=-z; lower_tail=!lower_tail; }
  double lp;
  if (!std::isfinite(z)) lp=((z>0)==lower_tail) ? 0 : special::kNegInf;
  else if (a==0) {
    lp=sn_detail::log_phi_cdf(lower_tail ? z : -z);
  } else if (z==0) {
    const double small=std::log(std::atan2(1.0,a))-std::log(M_PI);
    lp=lower_tail ? small : sn_detail::log_complement(small);
  } else if (z>0 && z<0.5) {
    // F(z)=erf(z/sqrt(2))+F(-z). This also resolves the half-normal
    // transition at scales much smaller than machine epsilon.
    const double thin=psn(-z,0,1,a,true,true);
    const double central=std::log(std::erf(z/std::sqrt(2.0)));
    const double large=std::max(thin,central),small=std::min(thin,central);
    const double below=large+std::log1p(std::exp(small-large));
    lp=lower_tail ? below : sn_detail::log_complement(below);
  } else if (z<0) {
    // Owen's difference loses all digits for large alpha or a thin tail.
    double p=special::pnorm(z,0,1,true,false)-2*special::owens_t(z,a);
    const double normal=special::pnorm(z,0,1,true,false);
    const double small=(p>1e-5*normal && z>-8 && a<20) ? std::log(p) : sn_detail::log_integral(z,a,true);
    lp=lower_tail ? small : sn_detail::log_complement(small);
  } else {
    const double small=z>8 ? sn_detail::log_integral(z,a,false) :
      std::log(special::pnorm(-z,0,1,true,false)+2*special::owens_t(z,a));
    lp=lower_tail ? sn_detail::log_complement(small) : small;
  }
  return log_p ? lp : std::exp(lp);
}
inline double qsn(double p,double xi=0,double omega=1,double alpha=0,bool lower_tail=true,bool log_p=false) {
  if (!sn_detail::valid(xi,omega,alpha) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return special::kNaN;
  // Exact symmetric member avoids rounded log-tail splitting at the median.
  if (alpha==0) return xi+omega*special::normal_quantile(p,lower_tail,log_p);
  double lp=log_p ? p : std::log(p);
  if (lp==special::kNegInf) return lower_tail ? special::kNegInf : -special::kNegInf;
  if (lp==0) return lower_tail ? -special::kNegInf : special::kNegInf;
  if (lp>-std::log(2.0)) { lp=std::log(-std::expm1(lp)); lower_tail=!lower_tail; }
  double lo=-1,hi=1;
  while ((lower_tail ? psn(lo,0,1,alpha,true,true)>lp : psn(lo,0,1,alpha,false,true)<lp)) lo*=2;
  while ((lower_tail ? psn(hi,0,1,alpha,true,true)<lp : psn(hi,0,1,alpha,false,true)>lp)) hi*=2;
  for(int i=0;i<1200;++i) {
    const double mid=lo+(hi-lo)/2;
    if(mid==lo || mid==hi) break;
    const double f=psn(mid,0,1,alpha,lower_tail,true);
    if(lower_tail ? f<lp : f>lp) lo=mid; else hi=mid;
  }
  return static_cast<double>(static_cast<CPPUNIV_EXTENDED_REAL>(xi)+static_cast<CPPUNIV_EXTENDED_REAL>(omega)*(lo+(hi-lo)/2));
}
template<typename Rng>
inline double rsn(Rng& rng,double xi=0,double omega=1,double alpha=0) {
  if (!sn_detail::valid(xi,omega,alpha)) return special::kNaN;
  const double den=std::hypot(1.0,alpha),delta=alpha/den;
  const double u=rng.rnorm(),v=rng.rnorm();
  return xi+omega*(delta*std::abs(u)+v/den);
}
}
}
