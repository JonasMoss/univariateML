#pragma once
#include "../../core/precision.hpp"
#include "../../special/cmp.hpp"
#include "../../special/tails.hpp"
#include "../pois/dpqr.hpp"
namespace cppunivariate {
namespace cmp_dpqr_detail {
inline bool valid(double lambda,double nu) {
 return std::isfinite(lambda) && lambda>=0 && std::isfinite(nu) && nu>=0 && (nu>0 || lambda<1);
}
inline double log_complement(double x) {
 return x < -0.6931471805599453 ? std::log1p(-std::exp(x)) : std::log(-std::expm1(x));
}
// Scale before forming x*log(x), which can overflow although nu*log(x!)
// is representable. Stirling's omitted correction is between 0 and nu/(12*x).
inline double diffuse_log_weight(double x,double nu) {
 if(x<1e305) return -nu*std::lgamma(x+1);
 const double lx=std::log(x);
 return -(nu*x)*(lx-1)-.5*nu*(lx+1.83787706640934548356);
}
// Unit-rate diffuse tail: Euler--Maclaurin with a bounded trapezoid
// remainder, plus positive Simpson integration with an analytic fourth-derivative bound.
inline double diffuse_tail(double start,double nu) {
 const double delta=nu*special::digamma(start+1), lg=std::lgamma(start+1);
 if(!(delta>0)) return special::kNaN;
 const auto f=[&](double y) {return std::exp(-nu*(std::lgamma(start+1+y/delta)-lg));};
 struct Panel {double a,b; int depth;};
 std::vector<Panel> stack{{0,80,0}};
 double integral=0,error=0;
 int work=0;
 while(!stack.empty() && work++<1000000) {
   const auto panel=stack.back();stack.pop_back();
   const double a=panel.a,b=panel.b,h=b-a, x=start+a/delta;
   const double A=nu/delta*std::log(start+1+b/delta);
   const double B=nu/(delta*delta)*(1/x+1/(x*x));
   const double C=nu/std::pow(delta,3)*(1/(x*x)+2/std::pow(x,3));
   const double D=nu/std::pow(delta,4)*(2/std::pow(x,3)+6/std::pow(x,4));
   const double bound=f(a)*(std::pow(A,4)+6*A*A*B+3*B*B+4*A*C+D)*std::pow(h,5)/2880;
   if(bound<=2e-13*h*f(b)) {
     integral+=h*(f(a)+4*f((a+b)/2)+f(b))/6;error+=bound;
   } else {
     if(panel.depth>=50) return special::kNaN;
     stack.push_back({(a+b)/2,b,panel.depth+1});stack.push_back({a,(a+b)/2,panel.depth+1});
   }
 }
 if(!stack.empty()) return special::kNaN;
 integral/=delta;error=(error+std::exp(-80))/delta;
 // |psi|<=log(x+1), psi'<=1/x+1/x^2; concavity of log f
 // supplies f(start+t)<=f(start)*exp(-delta*t).
 const double L=std::log(start+1), u=1/(start+1);
 const double rem=(nu*nu*(L*L/delta+2*L*u/(delta*delta)+2*u*u/std::pow(delta,3))+
                   nu*(1/start+1/(start*start))/delta)/12;
 const double value=integral+.5;
 if((rem+error)>value*2e-12) return special::kNaN;
 return -nu*lg+std::log(value);
}
inline double diffuse_log_z(double nu) {
 // One-entry cache; scalar binding calls commonly repeat the same law.
 static thread_local double cached_nu=special::kNaN,cached_z=special::kNaN;
 if(nu==cached_nu) return cached_z;
 double sum=0,correction=0;
 for(int k=0;k<100000;++k) {
   const double y=std::exp(-nu*std::lgamma(k+1.0))-correction,next=sum+y;
   correction=(next-sum)-y;sum=next;
 }
 cached_z=special::log_sum_exp(std::log(sum),diffuse_tail(100000,nu));
 cached_nu=nu;return cached_z;
}
struct Law {
 double lambda,nu,eta;
 CmpMoments moments;
 double diffuse_z=special::kNaN;
 Law(double l,double n):lambda(l),nu(n),eta(std::log(l)) {
  if(l==1 && n>0 && n<=1e-7) diffuse_z=diffuse_log_z(n);
  else if (l>0 && n>0) moments=cmp_moments(eta,n);
 }
 double density(double x,bool log) const {
  if (x<0 || !std::isfinite(x) || x!=std::floor(x)) return log ? special::kNegInf : 0;
  CPPUNIV_EXTENDED_REAL lp;
  if (lambda==0) lp=x==0 ? 0 : special::kNegInf;
  else if (nu==0) lp=x*static_cast<CPPUNIV_EXTENDED_REAL>(eta)+std::log1p(-lambda);
  else if(std::isfinite(diffuse_z)) lp=diffuse_log_weight(x,nu)-diffuse_z;
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
  if(std::isfinite(diffuse_z)) {
   double small;bool down=q<100000;
   if(down) {
     double sum=0,correction=0;
     for(int k=0;k<=q;++k) {
       const double y=std::exp(-nu*std::lgamma(k+1.0))-correction,next=sum+y;
       correction=(next-sum)-y;sum=next;
     }
     small=std::log(sum)-diffuse_z;
   } else if(q>1e40) {
     const double first=diffuse_log_weight(q+1,nu)-diffuse_z;
     const double width=-std::log(-std::expm1(-nu*std::log(q+2)));
     if(width>1e-12*std::max(1.0,std::abs(first))) return special::kNaN;
     small=first;
   } else small=diffuse_tail(q+1,nu)-diffuse_z;
   return special::tail_probability(special::tails_from_log(small,down),lower,log);
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
 if (nu==1) return dpois(x,lambda,log);
 return cmp_dpqr_detail::Law(lambda,nu).density(x,log);
}
inline double pcmp(double q,double lambda,double nu,bool lower_tail=true,bool log_p=false) {
 if (!cmp_dpqr_detail::valid(lambda,nu) || std::isnan(q)) return special::kNaN;
 if (nu==1) return ppois(q,lambda,lower_tail,log_p);
 return cmp_dpqr_detail::Law(lambda,nu).cdf(q,lower_tail,log_p);
}
inline double qcmp(double p,double lambda,double nu,bool lower_tail=true,bool log_p=false) {
 if (!cmp_dpqr_detail::valid(lambda,nu)) return special::kNaN;
 if (nu==1) return qpois(p,lambda,lower_tail,log_p);
 return cmp_dpqr_detail::Law(lambda,nu).quantile(p,lower_tail,log_p);
}
template <typename Rng>
inline double rcmp(Rng &rng,double lambda,double nu) {return qcmp(rng.runif(0,1),lambda,nu);}
}
}
