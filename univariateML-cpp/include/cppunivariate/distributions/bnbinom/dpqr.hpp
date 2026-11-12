// Beta negative binomial: gamma-ratio masses and controlled beta-mixture tails.
#pragma once
#include "../../core/precision.hpp"
#include "../../special/special.hpp"
#include "../../special/tails.hpp"
#include <algorithm>
#include <cmath>
#include <limits>

namespace cppunivariate {
namespace bnbinom_detail {
inline CPPUNIV_EXTENDED_REAL stirling(CPPUNIV_EXTENDED_REAL z) {
  const CPPUNIV_EXTENDED_REAL t = 1/z, t2 = t*t;
  return t*(CPPUNIV_EXTENDED_LITERAL(1.,1.L)/12-t2*(CPPUNIV_EXTENDED_LITERAL(1.,1.L)/360-t2*(CPPUNIV_EXTENDED_LITERAL(1.,1.L)/1260-t2*(CPPUNIV_EXTENDED_LITERAL(1.,1.L)/1680-t2/1188))));
}
// log Gamma(z+h)/Gamma(z), avoiding subtraction of large log gammas.
inline CPPUNIV_EXTENDED_REAL increment(CPPUNIV_EXTENDED_REAL z, CPPUNIV_EXTENDED_REAL h) {
  CPPUNIV_EXTENDED_REAL correction = 0;
  while (z < 16) { correction -= std::log1p(h/z); z += 1; }
  return correction + h*std::log(z) + (z+h-CPPUNIV_EXTENDED_LITERAL(.5,.5L))*std::log1p(h/z)-h +
    stirling(z+h)-stirling(z);
}
inline CPPUNIV_EXTENDED_REAL zero(CPPUNIV_EXTENDED_REAL r, CPPUNIV_EXTENDED_REAL a, CPPUNIV_EXTENDED_REAL b) {
  if (r > b) std::swap(r,b);
  // Mixed Taylor difference of log Gamma. For (r+b)/a <= 1e-3,
  // the polygamma series gives a geometric majorant for omitted terms.
  if (a>=1e6 && (r+b)/a<=1e-3) {
    const CPPUNIV_EXTENDED_REAL u=r/a,v=b/a;
    CPPUNIV_EXTENDED_REAL total=0;
    for(int n=2;n<=12;++n) {
      CPPUNIV_EXTENDED_REAL cross=0,choose=1;
      for(int j=1;j<n;++j) {
        choose*=static_cast<CPPUNIV_EXTENDED_REAL>(n-j+1)/j;
        cross+=choose*std::pow(u,j)*std::pow(v,n-j);
      }
      const CPPUNIV_EXTENDED_REAL coefficient=a/(n*(n-1.0))+0.5/n;
      total+=(n%2?1:-1)*cross*coefficient;
    }
    // The first mixed Stirling correction has magnitude rb/(6a^3), with
    // relative error O((r+b)/a); include its convergent binomial series.
    CPPUNIV_EXTENDED_REAL corr=0;
    for(int n=2;n<=12;++n)
      corr+=(n%2?-1:1)*(std::pow(u+v,n)-std::pow(u,n)-std::pow(v,n));
    return total-corr/(12*a);
  }
  CPPUNIV_EXTENDED_REAL correction = 0;
  while (a < 16) {
    correction += std::log(a/(a+r)) + std::log1p(r/(a+b));
    a += 1;
  }
  return correction - r*std::log1p(b/a) +
    (a+r-CPPUNIV_EXTENDED_LITERAL(.5,.5L))*std::log1p(r/a)-(a+b+r-CPPUNIV_EXTENDED_LITERAL(.5,.5L))*std::log1p(r/(a+b)) +
    stirling(a+r)-stirling(a)-stirling(a+b+r)+stirling(a+b);
}
inline double logpmf(int x, double r, double a, double b) {
  return static_cast<double>(zero(r,a,b)+increment(r,x)+increment(b,x)-
    increment(static_cast<CPPUNIV_EXTENDED_REAL>(a)+r+b,x)-special::lgamma(x+1.0));
}
inline bool valid(double r, double a, double b) {
  return r>0 && a>0 && b>0 && std::isfinite(r) && std::isfinite(a) && std::isfinite(b);
}
}
namespace bnbinom_detail {
inline double unit_log_ratio(double a,double b) {
  const double ratio=a/b;
  if(std::isnormal(ratio))
    return std::log(ratio)+std::log1p((std::fma(-ratio,b,a)/b)/ratio);
  return std::log(a)-std::log(b);
}
// S(k)=product_{j=0}^k (r+j)/(r+a+j). Scale its positive
// log-hazard terms by the first term and compensate their finite sum.
inline double unit_log_hazard(int cells,double r,double a) {
  const double anchor=special::log_softplus_product(1,unit_log_ratio(a,r));
  double sum=0,correction=0;
  for(int j=0;j<cells;++j) {
    const double term=std::exp(special::log_softplus_product(1,unit_log_ratio(a,r+j))-anchor);
    const double adjusted=term-correction,next=sum+adjusted;
    correction=(next-sum)-adjusted;sum=next;
  }
  return anchor+std::log(sum);
}
inline double log_mass(double x,double r,double a,double b) {
  // The two count-shape coordinates are interchangeable.
  if(r==1) std::swap(r,b);
  if(a==1 && b==1) {
    const double lr=std::log(r), lx=x==0?special::kNegInf:std::log(x);
    const double ls=special::log_sum_exp(lr,lx);
    return (lr-ls)-special::log_sum_exp(ls,0);
  }
  if(b==1 && x<100000) {
    const double preceding=x==0?0:std::exp(unit_log_hazard(static_cast<int>(x),r,a));
    return -preceding-special::softplus(-unit_log_ratio(a,r+x));
  }
  if(b==1 && x>=100) {
    const double first=special::log_beta_positive(x+r,a+1), second=special::log_beta_positive(r,a);
    const double value=first-second;
    if(16*std::numeric_limits<double>::epsilon()*(std::abs(first)+std::abs(second))>1e-10*std::max(1.0,std::abs(value))) return special::kNaN;
    return value;
  }
  if(r>=1 && b>=1 && (r+b)/a<=1e-3) {
    // Rising-factorial form: keep the huge common alpha gamma factors
    // cancelled before evaluation. Reject cancellation beyond the practical
    // floating-point margin; a far-count gamma-ratio chart follows below.
    const CPPUNIV_EXTENDED_REAL R=r,A=a,B=b,X=x;
    const CPPUNIV_EXTENDED_REAL z=zero(R,A,B),u=increment(R,X),v=increment(B,X),
      w=increment(A+R+B,X),f=std::lgamma(X+1);
    const CPPUNIV_EXTENDED_REAL value=z+u+v-w-f;
    const CPPUNIV_EXTENDED_REAL work=std::abs(z)+std::abs(u)+std::abs(v)+std::abs(w)+std::abs(f);
    const double result=static_cast<double>(value);
    const double target=1e-10*(result>=-745?1:std::max(1.0,std::abs(result)));
    const CPPUNIV_EXTENDED_REAL error=32*std::numeric_limits<CPPUNIV_EXTENDED_REAL>::epsilon()*work+
      4*std::numeric_limits<double>::epsilon()*std::max(1.0,std::abs(result))+8e-15;
    if(result<=0 && std::isfinite(result) && error<target) return result;
    const double g1=special::lgamma(a+r),g2=special::lgamma(a+b),g3=special::lgamma(r),
      g4=special::lgamma(a),g5=special::lgamma(b),
      c1=special::log_gamma_ratio(x,r,1),c2=special::log_gamma_ratio(x,b,a+r+b);
    const double far=g1+g2-g3-g4-g5+c1+c2;
    const double far_work=std::abs(g1)+std::abs(g2)+std::abs(g3)+std::abs(g4)+std::abs(g5)+std::abs(c1)+std::abs(c2);
    if(far<=0 && std::isfinite(far) && 32*std::numeric_limits<double>::epsilon()*far_work<
       1e-10*(far>=-745?1:std::max(1.0,std::abs(far)))) return far;
    return special::kNaN;
  }
  if(x<100) return logpmf(static_cast<int>(x),r,a,b);
  return special::lgamma(a+r)+special::lgamma(a+b)-special::lgamma(r)-special::lgamma(a)-special::lgamma(b)
    +special::log_gamma_ratio(x,r,1)+special::log_gamma_ratio(x,b,a+r+b);
}
inline special::LogTails tails(double x,double r,double a,double b) {
  if(x<0) return special::tails_from_log(special::kNegInf,true);
  if(x==special::kPosInf) return special::tails_from_log(special::kNegInf,false);
  x=std::floor(x);
  if(b==1 || r==1) {
    const double rr=b==1?r:b;
    if(x==0) {
      const double odds=special::log_positive_ratio(a,rr);
      const double lower=-special::softplus(-odds),upper=-special::softplus(odds);
      return lower<upper?special::tails_from_log(lower,true):special::tails_from_log(upper,false);
    }
    if(a==1) {
      const double odds=special::log_positive_ratio(x+1,rr);
      const double lower=-special::softplus(-odds), upper=-special::softplus(odds);
      return lower<upper?special::tails_from_log(lower,true):special::tails_from_log(upper,false);
    }
    if(x<100000)
      return special::tails_from_log_hazard(unit_log_hazard(static_cast<int>(x)+1,rr,a),false);
    if(x+1+rr==rr) return {special::kNaN,special::kNaN,false};
    double upper;
    if(a<16) upper=static_cast<double>(increment(rr,a)-increment(x+1+rr,a));
    else {
      const double first=special::log_beta_positive(x+1+rr,a),second=special::log_beta_positive(rr,a);
      upper=first-second;
      if(16*std::numeric_limits<double>::epsilon()*(std::abs(first)+std::abs(second))>1e-10*std::max(1.0,std::abs(upper)))
        return {special::kNaN,special::kNaN,false};
    }
    if(upper>=0 || -upper<std::numeric_limits<double>::denorm_min()/1e-12)
      return {special::kNaN,special::kNaN,false};
    return special::tails_from_log(upper,false);
  }
  if(r>=1 && b>=1 && (r+b)/a<=1e-3 && x<10000) {
    // Positive upper sum. For j>=k, R_j <= (j+D)/(j+E),
    // D=r+b-1+(r-1)(b-1)/(k+1), E=a+r+b. Gauss's
    // telescoping sum bounds the remaining tail by
    // p_k R_k (k+E)/(E-D-1) whenever E>D+1.
    double mass=log_mass(0,r,a,b),lower=mass;
    for(int j=0;j<x;++j) {
      mass+=std::log((r+j)/(j+1.0))+std::log((b+j)/(a+r+b+j));
      lower=special::log_sum_exp(lower,mass);
    }
    if(lower<-.6931471805599453) return special::tails_from_log(lower,true);
    double upper=special::kNegInf;
    for(int k=static_cast<int>(x)+1;k<100000;++k) {
      mass+=std::log((r+k-1)/k)+std::log((b+k-1)/(a+r+b+k-1));
      upper=special::log_sum_exp(upper,mass);
      const double E=a+r+b,D=r+b-1+(r-1)*((b-1)/(k+1));
      if(E>D+1) {
        const double ratio=((r+k)/(k+1.0))*((b+k)/(E+k));
        const double remainder=mass+std::log(ratio)+std::log((k+E)/(E-D-1));
        if(remainder<upper+std::log(2e-14)) return special::tails_from_log(upper,false);
      }
    }
    return {special::kNaN,special::kNaN,false};
  }
  if(x<300) {
    double lm=log_mass(0,r,a,b), sum=lm;
    for(int j=0;j<x;++j) {
      lm+=std::log((r+j)/(j+1.0))+std::log((b+j)/(a+r+b+j));
      sum=special::log_sum_exp(sum,lm);
    }
    if(sum<-.6931471805599453) return special::tails_from_log(sum,true);
    // Move the expansion away from n=1, where its majorant decays
    // slowly, then add the intervening positive masses without subtraction.
    double upper=tails(300,r,a,b).upper;
    for(int j=static_cast<int>(x);j<300;++j) {
      lm+=std::log((r+j)/(j+1.0))+std::log((b+j)/(a+r+b+j));
      upper=special::log_sum_exp(upper,lm);
    }
    return special::tails_from_log(upper,false);
  }
  // Swapped beta integrals give a 3F2 at one. After j>=b-1,
  // the remaining absolute sum is bounded by |next|*(j+n+r+a)/(n-1).
  // Use symmetry to minimize alternating cancellation (b is the smaller).
  if(b>r) std::swap(b,r);
  const double n=x+1;
  double term=1,total=1;
  for(int j=0;j<100000;++j) {
    const double next=term*((a+j)/(a+1+j))*((1-b+j)/(j+1.0))*((r+a+j)/(n+r+a+j));
    if(next==0 || (j>=b-1 && std::abs(next)*((j+n+r+a)/(n-1))<std::abs(total)*2e-16)) {
      const double pref=special::log_gamma_ratio(n+r,0,a)+special::log_gamma_ratio(r,a,0)
          -(std::log(a)+special::log_beta_positive(a,b));
      return special::tails_from_log(pref+std::log(total),false);
    }
    total+=next;term=next;
  }
  return {special::kNaN,special::kNaN,false};
}
}
namespace distributions {
inline double dbnbinom(double x,double size,double alpha,double beta,bool log) {
  if(!bnbinom_detail::valid(size,alpha,beta) || std::isnan(x)) return special::kNaN;
  if(x<0 || !std::isfinite(x) || x!=std::floor(x)) return log ? special::kNegInf : 0;
  const double l=bnbinom_detail::log_mass(x,size,alpha,beta);
  return log ? l : std::exp(l);
}
inline double pbnbinom(double q,double size,double alpha,double beta,bool lower_tail,bool log_p) {
  if(!bnbinom_detail::valid(size,alpha,beta) || std::isnan(q)) return special::kNaN;
  return special::tail_probability(bnbinom_detail::tails(q,size,alpha,beta),lower_tail,log_p);
}
inline double qbnbinom(double p,double size,double alpha,double beta,bool lower_tail,bool log_p) {
  if(!bnbinom_detail::valid(size,alpha,beta)) return special::kNaN;
  return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),0,special::kPosInf,
    [&](double x) {return bnbinom_detail::tails(x,size,alpha,beta);},p,lower_tail,log_p);
}
template <typename Rng>
inline double rbnbinom(Rng &rng, double size, double alpha, double beta) {
  return rng.rpois(rng.rgamma(size,1)*rng.rgamma(beta,1)/rng.rgamma(alpha,1));
}
}
}
