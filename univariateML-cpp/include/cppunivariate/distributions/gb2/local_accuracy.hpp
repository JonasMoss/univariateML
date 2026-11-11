#pragma once
#include "../sstd/local_accuracy.hpp"
#include "../../core/span.hpp"
namespace cppunivariate { namespace gb2_accuracy {
// log(1+x)-x, differentiated without cancellation. The polynomial remainder
// and its first two logarithmic derivatives are below binary64 roundoff on
// [0,1/8]. This is a function evaluation, not a fitting iteration cap.
inline local_accuracy::Jet log1pmx(const local_accuracy::Jet& x){
 return local_accuracy::log1pmx(x);
}
inline double spacing(double x){x=std::abs(x);return std::nextafter(x,INFINITY)-x;}
inline double log_ratio(double x,double b){return x>=b*.5&&x<=b*2?std::log1p((x-b)/b):std::log(x)-std::log(b);}
// Center the beta kernel at its fitted logit mean before differentiating.
// exp(d)-1-d is evaluated by its convergent series near zero.
inline local_accuracy::Jet expm1mx(const local_accuracy::Jet& d){
 if(std::abs(d.v)>.5)return local_accuracy::exp(d)-1-d;
 local_accuracy::Jet term=d*d/2,sum=term;
 for(int k=3;k<=24;++k){term=term*d/k;sum=sum+term;}
 return sum;
}
inline local_accuracy::Jet centered_kernel(const local_accuracy::Jet& p,const local_accuracy::Jet& q,const local_accuracy::Jet& d){
 if(std::abs(d.v)>.5){auto eta=local_accuracy::log(p)-local_accuracy::log(q),h=eta+d;
  return p*(local_accuracy::softplus(-h)-local_accuracy::softplus(-eta))+q*(local_accuracy::softplus(h)-local_accuracy::softplus(eta));}
 const auto remainder=expm1mx(d),s=p+q,r=p/s*(d+remainder);
 // Here |r| < exp(.5)-1; use the series only in its proven range.
 const auto log_remainder=std::abs(r.v)<=.125?local_accuracy::log1pmx(r):local_accuracy::log1p(r)-r;
 return s*log_remainder+p*remainder;
}
inline double scaled_trigamma(double x){
 if(x<100)return x*x*(special::trigamma(x)-1/x);
 const double r=1/x,r2=r*r;return .5+r*(1./6+r2*(-1./30+r2*(1./42-r2/30)));
}
// Propagate logit/probability rounding through beta mean/precision information.
// The correlated data score (1/u,-1/(1-u)) is transformed BEFORE inversion;
// separate shape-score bounds would lose the concentration information.
inline double input_error(span probability,span uncertainty,double p,double q){
 const double t=p+q,mu=p/t,mc=q/t,n=probability.size();
 const double kp=scaled_trigamma(p),kq=scaled_trigamma(q),kt=scaled_trigamma(t);
 const double aa=n*(p*mc+mc*mc*kp+mu*mu*kq),ab=n*(mc*kp-mu*kq),bb=n*(kp+kq-kt),det=aa*bb-ab*ab;
 if(!(det>0)||!std::isfinite(det))return INFINITY;
 double ep=0,eq=0;
 for(std::size_t i=0;i<probability.size();++i){
  const double u=probability[i],factor=uncertainty[i]/(u*(1-u));
  const double a=p*mc*factor,b=-t*(u-mu)*factor;
  const double eta=(bb*a-ab*b)/det,tau=(-ab*a+aa*b)/det;
  ep+=std::abs(mc*eta+tau);eq+=std::abs(tau-mu*eta);
 }
 return 2*std::max(ep,eq);
}
// log B(p,q)+p log q. This removes the leading gamma-limit term BEFORE
// differentiation, rather than subtracting O(p) shape scores afterwards.
inline local_accuracy::Jet rare_normalizer(const local_accuracy::Jet& p,const local_accuracy::Jet& q){
 const auto r=p/q;
 return local_accuracy::lgamma(p)-q*gb2_accuracy::log1pmx(r)-(p-.5)*local_accuracy::log1p(r)-
   local_accuracy::stirling(q+p)+local_accuracy::stirling(q);
}
} }
