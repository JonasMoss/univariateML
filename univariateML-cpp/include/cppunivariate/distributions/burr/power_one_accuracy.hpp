#pragma once
#include <algorithm>
#include "../../core/span.hpp"
#include "../../core/work_budget.hpp"
#include <array>
#include <cmath>
#include <limits>
namespace cppunivariate { namespace burr_power_one_detail {
// Error-free binary64 sums/products retain the small departure from the
// exponential face. No extended hardware precision is needed.
struct Pair {double hi=0,lo=0;Pair()=default;Pair(double v):hi(v){};};
inline Pair normalize(double a,double b){Pair r;r.hi=a+b;r.lo=b-(r.hi-a);return r;}
inline Pair operator+(Pair a,Pair b){const double s=a.hi+b.hi,v=s-a.hi,e=(a.hi-(s-v))+(b.hi-v);return normalize(s,e+a.lo+b.lo);}
inline Pair operator-(Pair a){a.hi=-a.hi;a.lo=-a.lo;return a;}
inline Pair operator-(Pair a,Pair b){return a+(-b);}
inline Pair operator*(Pair a,Pair b){const double p=a.hi*b.hi,e=std::fma(a.hi,b.hi,-p);return normalize(p,e+a.hi*b.lo+a.lo*b.hi+a.lo*b.lo);}
inline Pair operator/(Pair a,Pair b){const double q=a.hi/b.hi;const Pair residual=a-Pair(q)*b;return Pair(q)+Pair((residual.hi+residual.lo)/b.hi);}
inline int sign(Pair a){return a.hi>0?1:a.hi<0?-1:a.lo>0?1:a.lo<0?-1:0;}
inline double value(Pair a){return a.hi+a.lo;}
inline Pair scaled(Pair a,int exponent){a.hi=std::scalbn(a.hi,exponent);a.lo=std::scalbn(a.lo,exponent);return a;}
inline Pair log_two(){Pair a(.693147180559945309417232121458176568);a.lo=2.319046813846299558417771099e-17;return a;}
// On [-1/2,1/2], the atanh expansion has remainder below 1e-39.
// Keeping a small increment separate avoids losing close raw spacings.
inline Pair log_increment(Pair a) {
  const Pair t=a/(Pair(2)+a),square=t*t;Pair term=t,sum=t;
  for(int j=1;j<40;++j){term=term*square;sum=sum+term/Pair(2*j+1);}
  return Pair(2)*sum;
}
inline Pair log_mantissa(Pair a,int& exponent) {
  std::frexp(a.hi,&exponent);return log_increment(scaled(a,-exponent)-Pair(1));
}
inline Pair logarithm(Pair a) {
  if(!(a.hi>0))return Pair(-INFINITY);
  int exponent=0;const auto mantissa=log_mantissa(a,exponent);
  return Pair(exponent)*log_two()+mantissa;
}
inline Pair log_ratio(Pair a,Pair b) {
  int ea=0,eb=0;const auto la=log_mantissa(a,ea),lb=log_mantissa(b,eb);
  return Pair(ea-eb)*log_two()+la-lb;
}
inline Pair exponential(Pair a,bool moment=false) {
  if(moment&&a.hi< -80)return Pair(0); // omitted moment contribution < 2e-35
  if(!std::isfinite(a.hi)||a.hi>710)return Pair(INFINITY);
  if(a.hi< -746)return Pair(0);
  const int exponent=static_cast<int>(std::round(value(a)/log_two().hi));
  const Pair r=a-Pair(exponent)*log_two();Pair term(1),sum(1);
  for(int j=1;j<=40;++j){term=term*r/Pair(j);sum=sum+term;}
  return scaled(sum,exponent);
}
inline bool fit(span x,double& k,double& scale,double tol,int maxit,bool inverse,double shift=0,double held_power=1) {
  if(x.empty()||tol<4*std::numeric_limits<double>::epsilon()||!(scale>0&&held_power>0)||
      !std::isfinite(scale)||!std::isfinite(held_power))return false;
  double extreme_value=x[0];for(double v:x)extreme_value=inverse?std::min(extreme_value,v):std::max(extreme_value,v);
  const double extreme=extreme_value-shift;
  const int exponent=-std::ilogb(extreme);
  const Pair normalized_extreme=Pair(std::scalbn(extreme_value,exponent))-Pair(std::scalbn(shift,exponent));
  std::array<Pair,34> moment{};
  for(double v:x){Pair y;
    if(held_power==1) {
      const Pair normalized_v=Pair(std::scalbn(v,exponent))-Pair(std::scalbn(shift,exponent));
      if(!std::isfinite(normalized_v.hi))return false;
      y=inverse?normalized_extreme/normalized_v:normalized_v/normalized_extreme;
    } else {
      const Pair raw=Pair(v)-Pair(shift),anchor=Pair(extreme_value)-Pair(shift);
      if(!(raw.hi>0&&anchor.hi>0)||!std::isfinite(raw.hi)||!std::isfinite(anchor.hi))return false;
      const Pair denominator=inverse?raw:anchor;
      const Pair difference=inverse?Pair(extreme_value)-Pair(v):Pair(v)-Pair(extreme_value);
      const int unit=-std::ilogb(denominator.hi);
      const Pair increment=scaled(difference,unit)/scaled(denominator,unit);
      const Pair logarithm_ratio=value(increment)>-.5?log_increment(increment):
        inverse?log_ratio(anchor,raw):log_ratio(raw,anchor);
      y=exponential(Pair(held_power)*logarithm_ratio,true);
    }
    if(!std::isfinite(y.hi))return false;
    Pair power(1);
    for(int j=0;j<34;++j){moment[j]=moment[j]+power/Pair(double(x.size()));power=power*y;}}
  std::array<Pair,31> coef{};
  for(int l=0;l<=30;++l){coef[l]=Pair((l%2)?1:-1)*Pair(l+1)/Pair(l+2)*moment[l+2];
    for(int j=0;j<=l;++j)coef[l]=coef[l]+Pair((l%2)?-1:1)*moment[j+1]*moment[l-j+1]/Pair(j+1);}
  const auto score=[&](Pair r){Pair f=coef[30];for(int j=29;j>=0;--j)f=f*r+coef[j];return f;};
  const auto slope=[&](Pair r){Pair f=Pair(30)*coef[30];for(int j=29;j>=1;--j)f=f*r+Pair(j)*coef[j];return f;};
  const double eps=std::numeric_limits<double>::epsilon();
  const double rounding=(held_power==1?1024:8192)*x.size()*eps*eps+1e-55;
  Pair initial=held_power==1?Pair(inverse?scale/extreme:extreme/scale):
    exponential(Pair(held_power)*(inverse?log_ratio(Pair(scale),Pair(extreme_value)-Pair(shift)):
      log_ratio(Pair(extreme_value)-Pair(shift),Pair(scale))),true);
  Pair lo(0),hi=initial;
  if(!(value(hi)>0&&value(hi)<.01))hi=Pair(.001);
  const auto derivative_bound=[&](Pair left,Pair right) {
    const Pair center=(left+right)/Pair(2);
    double first=0,second=0,power=1;
    for(int l=1;l<=30;++l){first+=l*std::abs(value(coef[l]))*power;power*=value(right);}
    power=1;
    for(int l=2;l<=30;++l){second+=l*(l-1)*std::abs(value(coef[l]))*power;power*=value(right);}
    // Taylor bounds hold on this bracket; no global unimodality is assumed.
    return value(slope(center))-second*value(right-left)*.5-2*rounding-32*eps*first;
  };
  bool bracketed=false;
  if(value(coef[0])< -rounding) {
    for(std::int64_t i=0;i<solver_budget(maxit)&&value(score(hi))<=rounding;++i)hi=hi*Pair(2);
    bracketed=value(hi)<.01&&value(score(hi))>rounding&&derivative_bound(lo,hi)>1e-50;
  } else if(derivative_bound(Pair(0),Pair(std::nextafter(.01,0.)))>0)return false;
  if(!bracketed) {
    // A second local maximum need not share a monotone path to the face.
    // Look for a signed, increasing-score box around the search nomination.
    if(!(value(initial)>0&&value(initial)<.01))return false;
    Pair center=initial;double fraction=.125;
    for(std::int64_t i=0;i<solver_budget(maxit);++i) {
      lo=center*Pair(1-fraction);hi=center*Pair(1+fraction);
      if(value(hi)>=.01)hi=Pair(std::nextafter(.01,0.));
      const double lower=derivative_bound(lo,hi);
      if(lower>1e-50&&value(score(lo))< -rounding&&value(score(hi))>rounding) {
        bracketed=true;initial=center;break;
      }
      const Pair d=slope(center);
      if(!(sign(d)>0))return false;
      Pair step=score(center)/d;
      if(std::abs(value(step))>value(center)*.25)step=Pair(sign(step)*.25)*center;
      const Pair next=center-step;
      if(!(value(next)>0&&value(next)<.01))return false;
      if(lower<=1e-50)fraction*=.5;
      else if(sign(next-center)==0)return false;
      else {center=next;fraction=.125;}
    }
  }
  if(!bracketed)return false;
  const double derivative=derivative_bound(lo,hi);
  bool closed=false;
  Pair trial=sign(initial)>0&&sign(hi-initial)>=0?initial:(lo+hi)/Pair(2);
  for(std::int64_t i=0;i<solver_budget(maxit);++i){const Pair f=score(trial);
    if(value(f)< -rounding)lo=trial;
    else if(value(f)>rounding)hi=trial;
    const double radius=(std::abs(f.hi)+std::abs(f.lo)+rounding+8*eps*std::abs(f.hi))/derivative;
    if(std::isfinite(radius)) {
      const Pair left=trial-Pair(radius),right=trial+Pair(radius);
      if(sign(left-lo)>0)lo=left;
      if(sign(hi-right)>0)hi=right;
    }
    if(sign(hi-lo)<0)return false;
    if(sign(lo)>0&&value((hi-lo)/lo)<=tol*.0625*std::min(1.,held_power)){closed=true;break;}
    const Pair d=slope(trial),next=sign(d)>0?trial-f/d:lo;
    trial=sign(next-lo)>0&&sign(hi-next)>0?next:(lo+hi)/Pair(2);
  }
  if(!closed)return false;
  const Pair r=lo+(hi-lo)/Pair(2);
  Pair A=moment[33]/Pair(33);
  for(int j=31;j>=0;--j)A=Pair((j%2)?-1:1)*moment[j+1]/Pair(j+1)+r*A;
  const Pair shape=Pair(1)/(r*A);const double proposed_k=value(shape);double proposed_scale=0;
  if(held_power==1) {
    const Pair solved=inverse?normalized_extreme*r:normalized_extreme/r;
    proposed_scale=std::scalbn(value(solved),-exponent);
    if(std::abs(value((Pair(std::scalbn(proposed_scale,exponent))-solved)/solved))>tol*.25)return false;
  } else {
    const Pair log_scale=logarithm(Pair(extreme_value)-Pair(shift))+
      Pair(inverse?1:-1)*logarithm(r)/Pair(held_power);
    proposed_scale=value(exponential(log_scale));
    if(!(proposed_scale>0)||!std::isfinite(proposed_scale)||
        std::abs(value(logarithm(Pair(proposed_scale))-log_scale))>tol*.25)return false;
  }
  if(!(proposed_k>0&&proposed_scale>0)||!std::isfinite(proposed_k)||!std::isfinite(proposed_scale)||
      std::abs(value((Pair(proposed_k)-shape)/shape))>tol*.25)return false;
  k=proposed_k;scale=proposed_scale;return true;
}
}}
