#pragma once
#include "../../special/constants.hpp"
#include <algorithm>
#include <cmath>
#include <limits>

namespace cppunivariate {
namespace emg_detail {
inline bool valid(double mu,double sigma,double lambda) {
  return std::isfinite(mu) && std::isfinite(sigma) && sigma>0 && std::isfinite(lambda) && lambda>0;
}
// Normal Mills ratio M(t)=Phi(-t)/phi(t). Laplace's continued fraction
// avoids the exp(t*t/2)*erfc(t/sqrt(2)) overflow/underflow pair.
inline long double mills(long double t) {
  if(t<12) return std::sqrt(std::acos(-1.0L)/2)*std::exp(t*t/2)*std::erfc(t/std::sqrt(2.0L));
  long double r=0;
  for(int k=40;k>=1;--k) r=k/(t+r);
  return 1/(t+r);
}
inline long double logmills(long double t) {
  if(t>=0) return std::log(mills(t));
  return t*t/2+std::log(std::sqrt(std::acos(-1.0L)/2)*std::erfc(t/std::sqrt(2.0L)));
}
inline long double logphi(long double z) { return -z*z/2-0.918938533204672741780329736406L; }
inline long double logPhi(long double z) {
  if(z<=0) return logphi(z)+logmills(-z);
  return std::log1p(-std::erfc(z/std::sqrt(2.0L))/2);
}
inline long double log1mexp(long double x) {
  return x< -0.693147180559945309417232121458L ? std::log1p(-std::exp(x)) : std::log(-std::expm1(x));
}
inline long double logadd(long double a,long double b) {
  return std::max(a,b)+std::log1p(std::exp(-std::abs(a-b)));
}
inline long double logterm(long double z,long double a) {
  const long double t=a-z;
  return t>=0 ? logphi(z)+logmills(t) : a*(a/2-z)+logPhi(-t);
}
// -M'(t)=1-t*M(t), evaluated without subtracting nearly equal numbers.
inline long double dmills(long double t) {
  if(t<12) return 1-t*mills(t);
  long double r=0;
  for(int k=40;k>=2;--k) r=k/(t+r);
  const long double h=1/(t+r);
  return h/(t+h);
}
inline long double lowerlog(long double z,long double a) {
  // For small a the difference M(-z)-M(a-z) cancels. Integrate -M'
  // over the short interval by an eight-point Gauss-Legendre rule.
  const long double t=-z;
  if((t>=0 ? a/std::max(1.0L,t) : a*std::max(1.0L,-t))<0.01L) {
    constexpr long double nodes[4]={.18343464249564980494L,.52553240991632898582L,.79666647741362673959L,.96028985649753623168L};
    constexpr long double weights[4]={.36268378337836198297L,.31370664587788728734L,.22238103445337447054L,.10122853629037625915L};
    auto logderivative=[&](long double u) {
      const long double au=a*u,b=z-au;
      // phi(z)*(-M'(-z+au)); this form also avoids exp(z*z/2)
      // for very large positive z and rates far below machine epsilon.
      if(b>0) return logadd(logphi(z),std::log(b)+au*(au/2-z)+logPhi(b));
      return logphi(z)+std::log(dmills(-b));
    };
    long double s=-std::numeric_limits<long double>::infinity();
    for(int k=0;k<4;++k) {
      const long double pair=logadd(logderivative((1-nodes[k])/2),logderivative((1+nodes[k])/2));
      s=logadd(s,std::log(weights[k])+pair);
    }
    return std::log(a)+s-std::log(2.0L);
  }
  if(z<=0) return logPhi(z)+log1mexp(logmills(a-z)-logmills(-z));
  const long double l=logPhi(z), b=logterm(z,a);
  return l+log1mexp(std::min(0.0L,b-l));
}
}
namespace distributions {
inline double demg(double x,double mu,double sigma,double lambda,bool log=false) {
  if(!emg_detail::valid(mu,sigma,lambda) || std::isnan(x)) return special::kNaN;
  if(!std::isfinite(x)) return log ? special::kNegInf : 0;
  const long double z=(static_cast<long double>(x)-mu)/sigma,a=static_cast<long double>(lambda)*sigma;
  const double l=static_cast<double>(std::log(static_cast<long double>(lambda))+emg_detail::logterm(z,a));
  return log ? l : std::exp(l);
}
inline double pemg(double q,double mu,double sigma,double lambda,bool lower_tail=true,bool log_p=false) {
  if(!emg_detail::valid(mu,sigma,lambda) || std::isnan(q)) return special::kNaN;
  if(!std::isfinite(q)) { const bool one=lower_tail ? q>0 : q<0; return log_p ? (one?0:special::kNegInf) : (one?1:0); }
  const long double z=(static_cast<long double>(q)-mu)/sigma,a=static_cast<long double>(lambda)*sigma;
  const long double l=emg_detail::lowerlog(z,a);
  const long double u=emg_detail::logadd(emg_detail::logPhi(-z),emg_detail::logterm(z,a));
  const long double own=lower_tail?l:u, other=lower_tail?u:l;
  const double ans=static_cast<double>(std::min(0.0L,own<other?own:emg_detail::log1mexp(other)));
  return log_p?ans:std::exp(ans);
}
inline double qemg(double p,double mu,double sigma,double lambda,bool lower_tail=true,bool log_p=false) {
  if(!emg_detail::valid(mu,sigma,lambda) || std::isnan(p) || (log_p?p>0:p<0||p>1)) return special::kNaN;
  if(p==(log_p?special::kNegInf:0)) return lower_tail?special::kNegInf:special::kPosInf;
  if(p==(log_p?0:1)) return lower_tail?special::kPosInf:special::kNegInf;
  double target=log_p?p:std::log(p);
  // Always invert the smaller tail, including when p was supplied near one.
  if(target> -0.6931471805599453) {target=std::log(-std::expm1(target));lower_tail=!lower_tail;}
  const double largest=std::numeric_limits<double>::max();
  auto endpoint=[&](long double v) {return static_cast<double>(std::max(-static_cast<long double>(largest),std::min(static_cast<long double>(largest),v)));};
  long double step=sigma;
  double lo=endpoint(static_cast<long double>(mu)-step),hi=endpoint(static_cast<long double>(mu)+step);
  for(int k=0;k<4096 && (lower_tail?pemg(lo,mu,sigma,lambda,true,true)>target:pemg(lo,mu,sigma,lambda,false,true)<target);++k) {
    if(lo== -largest) return special::kNegInf;
    step*=2;lo=endpoint(static_cast<long double>(mu)-step);
  }
  step=sigma;
  for(int k=0;k<4096 && (lower_tail?pemg(hi,mu,sigma,lambda,true,true)<target:pemg(hi,mu,sigma,lambda,false,true)>target);++k) {
    if(hi==largest) return special::kPosInf;
    step*=2;hi=endpoint(static_cast<long double>(mu)+step);
  }
  double x=lo/2+hi/2;
  for(int k=0;k<200;++k) {
    const double v=pemg(x,mu,sigma,lambda,lower_tail,true);
    if(lower_tail?v<target:v>target) lo=x; else hi=x;
    if(static_cast<long double>(hi)-lo<2e-14L*std::max(static_cast<long double>(sigma),std::abs(static_cast<long double>(x)-mu))) break;
    const double deriv=std::exp(demg(x,mu,sigma,lambda,true)-v)*(lower_tail?1:-1);
    double next=x-(v-target)/deriv;
    if(!std::isfinite(next) || next<=lo || next>=hi) next=lo/2+hi/2;
    x=next;
  }
  return lo/2+hi/2;
}
template <typename Rng>
inline double remg(Rng &rng,double mu,double sigma,double lambda) {
  if(!emg_detail::valid(mu,sigma,lambda)) return special::kNaN;
  return mu+sigma*rng.rnorm()-std::log1p(-rng.runif(0,1))/lambda;
}
}
}
