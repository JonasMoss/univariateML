#pragma once
#include "../../core/precision.hpp"
#include "../../core/work_budget.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <cstdint>
namespace cppunivariate { namespace binomial_accuracy {
// Two-component arithmetic is used only for signs of the cleared held-pi
// score. Near a multiple root ordinary subtraction loses the coefficient
// accuracy, even though a narrow computed bracket appears to have closed.
struct Pair { double hi=0,lo=0; Pair(double x=0):hi(x){} Pair(double x,double y):hi(x),lo(y){} };
inline Pair add(Pair a,Pair b) {
  const double s=a.hi+b.hi,v=s-a.hi;
  const double e=(a.hi-(s-v))+(b.hi-v)+a.lo+b.lo;
  const double t=s+e;return {t,e-(t-s)};
}
inline Pair neg(Pair a){return {-a.hi,-a.lo};}
inline Pair mul(Pair a,Pair b) {
  const double p=a.hi*b.hi;
  const double e=std::fma(a.hi,b.hi,-p)+a.hi*b.lo+a.lo*b.hi+a.lo*b.lo;
  const double t=p+e;return {t,e-(t-p)};
}
inline Pair divide(Pair x,Pair y) {
  const double leading=x.hi/y.hi;
  const auto residual=add(x,neg(mul(y,Pair(leading))));
  return add(Pair(leading),Pair((residual.hi+residual.lo)/y.hi));
}
inline double magnitude(Pair x){return std::abs(x.hi)+std::abs(x.lo);}
inline Pair power(Pair a,double n) {
  Pair p(1);
  // Integer exponentiation, one pass per bit of the supplied double size.
  while(n>=1){const double h=std::floor(n/2);if(n-2*h==1)p=mul(p,a);n=h;if(n>=1)a=mul(a,a);}
  return p;
}
struct ScaledPair { Pair mantissa; int exponent=0; };
inline ScaledPair normalize(ScaledPair x) {
  if(x.mantissa.hi==0)return {Pair(0),-4096};
  const int shift=std::ilogb(std::abs(x.mantissa.hi))+1;
  x.mantissa={std::ldexp(x.mantissa.hi,-shift),std::ldexp(x.mantissa.lo,-shift)};
  x.exponent+=shift;
  // Below this exponent the contribution is negligible even compared with
  // the smallest positive binary64 inflation, including the score margin.
  if(x.exponent < -4096)return {Pair(0),-4096};
  return x;
}
inline ScaledPair scaled_product(ScaledPair a,ScaledPair b) {
  return normalize({mul(a.mantissa,b.mantissa),a.exponent+b.exponent});
}
inline ScaledPair scaled_power(Pair a,double n) {
  ScaledPair p{Pair(1),0},base=normalize({a,0});
  while(n>=1) {
    const double h=std::floor(n/2);
    if(n-2*h==1)p=scaled_product(p,base);
    n=h;if(n>=1)base=scaled_product(base,base);
  }
  return p;
}
inline double held_score(double p,double N,double pi,double S,double n,double k) {
  const Pair u=mul(Pair(N),Pair(p));
  const auto q=scaled_power(add(Pair(1),Pair(-p)),N);
  const int e=std::ilogb(pi)+1,common=std::max(e,q.exponent);
  const Pair inflation(std::ldexp(pi,-common));
  const Pair positive(std::ldexp(q.mantissa.hi,q.exponent-common),std::ldexp(q.mantissa.lo,q.exponent-common));
  const Pair a=mul(inflation,add(Pair(S),neg(mul(Pair(k),u))));
  const Pair b=mul(mul(add(Pair(1),Pair(-pi)),positive),add(Pair(S),neg(mul(Pair(n),u))));
  const Pair score=add(a,b);
  const double pad=128*std::numeric_limits<double>::epsilon()*std::numeric_limits<double>::epsilon()*
    (1+std::abs(a.hi)+std::abs(b.hi))*(1+N);
  const double value=score.hi+score.lo;
  return std::abs(value)>pad?value:0;
}
inline bool ratio_accuracy(double numerator,double denominator,double v,double tol);
inline int critical_probability(double& p,double N,double pi,double S,double n,double k,double tol) {
  const double z=n-k,U=S*(N*(n+k)+z),V=2*n*k*N*N;
  if(!(U>0&&V>0&&U<9007199254740992.&&V<9007199254740992.))return 0;
  const double logbound=2+std::log2(n)+std::log2(k)+std::log2(S)+std::log2(S+z)+2*std::log2(N);
  if(!(2*std::log2(U)<96&&logbound<96))return 0;
  auto right=mul(mul(Pair(4),Pair(n)),Pair(k));
  right=mul(mul(right,mul(Pair(S),Pair(S+z))),mul(Pair(N),Pair(N)));
  const auto disc=add(mul(Pair(U),Pair(U)),neg(right));
  if(disc.hi!=0||disc.lo!=0)return 0;
  const auto divisor=std::gcd(static_cast<std::uint64_t>(U),static_cast<std::uint64_t>(V));
  const double a=U/divisor,b=V/divisor;
  if(!(a>0&&a<b))return 0;
  double P=0,M=0;int bits=0;
  for(;bits<=std::numeric_limits<double>::digits;++bits) {
    P=std::ldexp(pi,bits);M=std::ldexp(1.,bits);
    if(P==std::floor(P))break;
  }
  if(bits>std::numeric_limits<double>::digits)return 0;
  const auto sb=mul(Pair(S),Pair(b));
  const auto first=add(sb,neg(mul(mul(Pair(k),Pair(N)),Pair(a))));
  const auto second=add(sb,neg(mul(mul(Pair(n),Pair(N)),Pair(a))));
  if(!(first.hi>0&&second.hi<0))return 0;
  const double mass=1+std::abs(first.hi)+std::abs(second.hi);
  if(!(N*std::log2(b)+bits+std::log2(mass)<96))return 0;
  const auto left_term=mul(mul(Pair(P),power(Pair(b),N)),first);
  const auto right_term=mul(mul(Pair(M-P),power(Pair(b-a),N)),second);
  const auto score=add(left_term,right_term);
  if(score.hi!=0||score.lo!=0)return 0;
  // A double turning point makes the ratio score monotone; its exact zero
  // is the unique maximum. All integer intermediates above have <100 bits.
  p=a/b;
  return ratio_accuracy(a,b,p,tol)?1:-1;
}
inline bool held_probability(double& p,double N,double pi,double S,double n,double k,double tol,int maxit) {
  if(p==0||p==1)return true;
  const double lower=(S/n)/N,upper=std::min(1.,(S/k)/N);
  const double eps=std::numeric_limits<double>::epsilon();
  // Algebraic endpoint score signs enclose every stationary probability.
  // At enormous size this already proves the absolute public-p bound.
  if(upper-lower+16*eps*upper<=tol/16) {
    p=std::max(lower,std::min(upper,p));return true;
  }
  const int critical=critical_probability(p,N,pi,S,n,k,tol);
  if(critical!=0)return critical>0;
  // Stay with the local maximum selected by the existing branch search.
  double radius=std::max(std::max(8*tol,1e-8)*p,std::numeric_limits<double>::denorm_min()),lo=0,hi=1;bool bracket=false;
  for(std::int64_t j=0;j<solver_budget(maxit);++j) {
    lo=std::max(0.,p-radius);hi=std::min(1.,p+radius);
    if(held_score(lo,N,pi,S,n,k)>0 && held_score(hi,N,pi,S,n,k)<0){bracket=true;break;}
    radius*=2;
  }
  if(!bracket)return false;
  for(std::int64_t j=0;j<solver_budget(maxit);++j) {
    const double mid=lo+(hi-lo)/2;
    if(hi-lo<=tol/16*std::max(lo,std::numeric_limits<double>::min())){if(!(p>=lo&&p<=hi))p=mid;return true;}
    if(mid==lo||mid==hi)return false;
    const double score=held_score(mid,N,pi,S,n,k);
    if(!std::isfinite(score))return false;
    if(score>0)lo=mid;else if(score<0)hi=mid;
    else {
      // A zero rounded score is not a sign. Test a full target-sized band.
      const double l=std::max(lo,mid-tol/32),h=std::min(hi,mid+tol/32);
      if(held_score(l,N,pi,S,n,k)>0&&held_score(h,N,pi,S,n,k)<0){if(!(p>=lo&&p<=hi))p=mid;return true;}
      return false;
    }
  }
  return false;
}
// Products of two binary64 numbers are retained exactly by the two
// components here. This checks a closed count ratio against the full public
// tolerance, including exactly represented sub-machine targets.
inline bool ratio_accuracy(double numerator,double denominator,double v,double tol) {
  if(tol>=1)return true;
  auto residual=add(Pair(numerator),neg(mul(Pair(v),Pair(denominator))));
  if(residual.hi<0||(residual.hi==0&&residual.lo<0))residual=neg(residual);
  const auto slack=add(mul(Pair(tol),Pair(denominator)),neg(residual));
  return slack.hi>0||(slack.hi==0&&slack.lo>=0);
}
inline Pair truncation_mass(Pair p,double N) {
  const auto q=scaled_power(add(Pair(1),neg(p)),N);
  return add(Pair(1),neg(Pair(std::ldexp(q.mantissa.hi,q.exponent),std::ldexp(q.mantissa.lo,q.exponent))));
}
struct PositiveRoot { Pair point; double relative_error=INFINITY; bool ok=false; };
inline PositiveRoot precise_positive_root(double p,double N,double S,double n,double tol,int maxit) {
  if(S==n)return {Pair(0),0,true};
  if(S==N*n)return {Pair(1),0,true};
  if(N==2) {
    const auto root=divide(Pair(2*(S-n)),Pair(S));
    const bool exact=root.lo==0 && std::fma(-root.hi,S,2*(S-n))==0;
    return {root,exact?0:64*std::numeric_limits<double>::epsilon()*std::numeric_limits<double>::epsilon(),true};
  }
  // Exact dyadic conditional means are closed cases, even when an ordinary
  // floating-point mean subtraction cannot resolve a sub-machine target.
  for(int bits=1;bits<=std::numeric_limits<double>::digits;++bits) {
    if(N*bits+std::log2(1+S+n*N)>=96)break;
    const double M=std::ldexp(1.,bits),a=std::round(p*M);
    if(!(a>0&&a<M))continue;
    const auto left=mul(mul(mul(Pair(n),Pair(N)),Pair(a)),power(Pair(M),N-1));
    const auto right=mul(Pair(S),add(power(Pair(M),N),neg(power(Pair(M-a),N))));
    const auto difference=add(left,neg(right));
    if(difference.hi==0&&difference.lo==0)return {Pair(a/M),0,true};
  }
  Pair point(p);double previous=INFINITY;
  const double eps=std::numeric_limits<double>::epsilon(),gamma=128*(N+1)*eps*eps;
  for(std::int64_t it=0;it<=solver_budget(maxit);++it) {
    const auto mass=truncation_mass(point,N);
    const auto q=scaled_power(add(Pair(1),neg(point)),N-1);
    const Pair q1(std::ldexp(q.mantissa.hi,q.exponent),std::ldexp(q.mantissa.lo,q.exponent));
    const auto a=mul(Pair(n),mul(Pair(N),point)),b=mul(Pair(S),mass);
    const auto score=add(a,neg(b));
    const auto d=mul(Pair(N),add(Pair(n),neg(mul(Pair(S),q1))));
    const double derivative=d.hi+d.lo;
    const double noise=gamma*(magnitude(a)+magnitude(b));
    const double dnoise=gamma*N*(n+S*magnitude(q1));
    if(!(derivative>4*dnoise) || !(point.hi>0&&point.hi<1))return {};
    const auto step=divide(score,d);
    const double denominator=std::max(point.hi,std::numeric_limits<double>::min());
    const double rounding=2*noise/(derivative-2*dnoise)/denominator;
    const double error=2*magnitude(step)/denominator+rounding;
    if(!std::isfinite(error)||error>.25)return {};
    if(std::isfinite(previous) && error<=std::max(tol/64,8*rounding) &&
       (previous<=std::max(tol/64,8*rounding)||error<=std::max(16*previous*previous,8*rounding)))
      return {point,error,true};
    if(it==solver_budget(maxit))break; // final permitted update was checked
    point=add(point,neg(step));previous=error;
  }
  return {};
}
inline bool project_probability(double& p,const PositiveRoot& root,double tol) {
  if(!root.ok)return false;
  p=root.point.hi+root.point.lo;
  return magnitude(add(root.point,Pair(-p)))+root.relative_error*std::abs(root.point.hi)<=tol;
}
inline bool precise_inflation(double& pi,Pair p,double N,double n,double k,double p_error,double tol) {
  const auto mass=truncation_mass(p,N);
  if(!(mass.hi>0))return false;
  const auto point=add(Pair(1),neg(divide(Pair(k),mul(Pair(n),mass))));
  if(point.hi<0){pi=0;return true;}
  pi=point.hi+point.lo;
  // Verify an exact dyadic transformation before applying a nonzero
  // arithmetic allowance to an exactly represented coefficient.
  if(p.lo==0 && p_error==0) {
    int pb=-1,ib=-1;
    for(int bits=0;bits<=std::numeric_limits<double>::digits;++bits) {
      if(pb<0 && std::ldexp(p.hi,bits)==std::floor(std::ldexp(p.hi,bits)))pb=bits;
      if(ib<0 && std::ldexp(pi,bits)==std::floor(std::ldexp(pi,bits)))ib=bits;
      if(pb>=0&&ib>=0)break;
    }
    if(pb>=0&&ib>=0&&N*pb+ib+std::log2(1+n+k)<96) {
      const auto residual=add(mul(mul(Pair(n),mass),add(Pair(1),Pair(-pi))),Pair(-k));
      if(residual.hi==0&&residual.lo==0)return true;
    }
  }
  const double eps=std::numeric_limits<double>::epsilon();
  const double noise=128*(N+1)*eps*eps/mass.hi;
  return magnitude(add(point,Pair(-pi)))+p_error+noise<=tol;
}
inline ExtendedReal positive_mean_excess(double N,ExtendedReal p) {
  if(p==0)return 0;
  if(p==1)return N-1;
  const ExtendedReal w=-std::log1p(-p),y=N*w;
  if(y<.125) {
    // Cancel the leading linear term symbolically, preserving the small
    // excess above one when almost all positive observations equal one.
    ExtendedReal series=ExtendedReal(1)/32;
    for(int j=31;j>=2;--j)series=ExtendedReal(1)/j+p*series;
    const ExtendedReal wm=p*p*series;
    ExtendedReal term=y*y/2,sum=term;
    for(int j=3;j<=20;++j){term*=-y/j;sum+=term;}
    return (sum-N*wm)/(-std::expm1(-y));
  }
  return N*p/(-std::expm1(-y))-1;
}
// Monotonicity is strict for N>1. Width is checked both absolutely for p
// and relatively to control a subsequent inflation transformation.
inline bool positive_probability(double& p,double N,double S,double n,double tol,int maxit) {
  if(S==n){p=0;return true;}if(S==N*n){p=1;return true;}
  if(tol<128*std::numeric_limits<double>::epsilon()) {
    // E[X | X>0] >= N*p encloses the positive root independently of
    // relative probability resolution. Inflation callers retain the full
    // precise root instead, since their transformation needs relative error.
    const double upper=(S/n)/N;
    if(upper*(1+4*std::numeric_limits<double>::epsilon())<=tol/16) {
      p=std::min(p,upper);return true;
    }
    return project_probability(p,precise_positive_root(p,N,S,n,tol,maxit),tol);
  }
  if(N==2){p=2*(S-n)/S;return ratio_accuracy(2*(S-n),S,p,tol);}
  const ExtendedReal target=(static_cast<ExtendedReal>(S)-n)/n;
  double lo=p/2,hi=std::min(1.,p*2);bool bracket=false;
  for(std::int64_t j=0;j<solver_budget(maxit);++j) {
    if(positive_mean_excess(N,lo)<target && positive_mean_excess(N,hi)>target){bracket=true;break;}
    lo/=2;hi=std::min(1.,hi*2);
  }
  if(!bracket)return false;
  for(std::int64_t j=0;j<solver_budget(maxit);++j) {
    const double mid=lo+(hi-lo)/2;
    if(hi-lo<=tol/32*std::max(lo,std::numeric_limits<double>::min())) {
      if(!(p>=lo&&p<=hi))p=mid;
      const ExtendedReal l=positive_mean_excess(N,std::max(0.,p-tol*p/16));
      const ExtendedReal h=positive_mean_excess(N,std::min(1.,p+tol*p/16));
      const ExtendedReal pad=64*std::numeric_limits<ExtendedReal>::epsilon()*(std::abs(target)+std::abs(l)+std::abs(h));
      return l+pad<target && h-pad>target;
    }
    if(mid==lo||mid==hi)return false;
    const ExtendedReal m=positive_mean_excess(N,mid);
    if(!std::isfinite(m))return false;
    if(m<target)lo=mid;else hi=mid;
  }
  return false;
}
} }
