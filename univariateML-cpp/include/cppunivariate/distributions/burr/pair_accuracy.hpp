#pragma once
#include "pair_polish.hpp"
#include <vector>
namespace cppunivariate { namespace burr_power_one_detail {
// Joint Weibull-limit coordinates: u=log(k), v=log(c),
// D=u-sign*c*log(s/unit). The weak u score retains its factor r=w/k;
// neither raw observation logarithms nor the leading Gamma terms are rounded
// before cancellation. All derivatives below are analytic in these coordinates.
inline bool joint_fit(span x,double& shape,double& power,double& scale,
    const std::array<bool,3>& free,double tol,int maxit,bool inverse,double shift,StopDetail& detail) {
  if(x.empty()||!(shape>0&&power>0&&scale>0)||(!std::isfinite(shape)||!std::isfinite(power)||!std::isfinite(scale)))return false;
  const double unit=scale,orientation=inverse?-1:1;
  std::vector<Pair> raw;
  for(double value:x){const Pair excess=Pair(value)-Pair(shift);if(!(excess.hi>0)||!std::isfinite(excess.hi))return false;raw.push_back(Pair(orientation)*log_ratio(excess,Pair(unit)));}
  std::array<Pair,3> state{{logarithm(Pair(shape)),logarithm(Pair(power)),logarithm(Pair(shape))}};
  const auto objective=[&](const std::array<Pair,3>& z) {
    PairEvaluation f;const Pair u=free[0]?z[0]:logarithm(Pair(shape)),k=free[0]?exponential(z[0]):Pair(shape),c=free[1]?exponential(z[1]):Pair(power),D=free[2]?z[2]:u;
    if(!(value(k)>0&&value(c)>0)||!std::isfinite(value(k)+value(c))){f.valid=false;return f;}
    for(const Pair l:raw) {
      const Pair A=c*l,h=D+A,w=exponential(h),r=w/k,den=Pair(1)+r;
      if(!std::isfinite(w.hi)||!std::isfinite(r.hi)){f.valid=false;return f;}
      const Pair G=value(r)<.5?log_increment(r):logarithm(den),B=r/den;
      Pair C,LM,term(1);
      if(value(r)<.125) {
        for(int j=0;j<64;++j){C=C+Pair(j%2?-1:1)*Pair(j+1)*term/Pair(j+2);term=term*r;}
        term=r*r;for(int j=2;j<=64;++j){LM=LM+Pair(j%2?1:-1)*term/Pair(j);term=term*r;}
      } else LM=G-r;
      const Pair departure=value(r)<.125?w*r*C:k*(G-B);
      const Pair gu=value(r)<.125?r*(Pair(1)/den-w*C):B-departure;
      const Pair M=(Pair(1)-w)/den,V=(w+r)/(den*den),gv=Pair(1)+A*M;
      const Pair Huu=value(r)<.125?r*((Pair(1)-w)/(den*den)+w*C):B/den+departure-k*B*B;
      const Pair HuD=r*(w-Pair(1))/(den*den),Huv=HuD*A,Hvv=V*A*A-A*M,HvD=V*A;
      const Pair ll=(free[1]?z[1]:logarithm(Pair(power)))+h-w-G-k*LM;
      f.likelihood=f.likelihood+ll;
      f.likelihood_magnitude+=std::abs(value(z[1]))+std::abs(value(h))+std::abs(value(w))+std::abs(value(G))+std::abs(value(k*LM));
      f.score[0]=f.score[0]+gu+(free[2]?Pair(0):M);f.score[1]=f.score[1]+gv;f.score[2]=f.score[2]+M;
      f.information[0][0]=f.information[0][0]+Huu+(free[2]?Pair(0):Pair(2)*HuD+V);f.information[0][1]=f.information[0][1]+Huv+(free[2]?Pair(0):HvD);f.information[0][2]=f.information[0][2]+HuD;
      f.information[1][1]=f.information[1][1]+Hvv;f.information[1][2]=f.information[1][2]+HvD;f.information[2][2]=f.information[2][2]+V;
      f.score_magnitude[0]+=value(r)<.125?std::abs(value(r))*(1+std::abs(value(w*C))):std::abs(value(B))+std::abs(value(departure));
      if(!free[2])f.score_magnitude[0]+=1+std::abs(value(w));
      f.score_magnitude[1]+=1+std::abs(value(A))*(1+std::abs(value(w)));
      f.score_magnitude[2]+=1+std::abs(value(w));
      f.information_magnitude+=1+std::abs(value(Huu))+std::abs(value(HuD))+std::abs(value(Huv))+std::abs(value(Hvv))+std::abs(value(HvD))+std::abs(value(V));
    }
    for(int i=0;i<3;++i)for(int j=0;j<i;++j)f.information[i][j]=f.information[j][i];
    f.projection[0][0]=Pair(free[0]?1:0);f.projection[1][1]=Pair(free[1]?1:0);
    if(free[2]) {f.projection[2][0]=Pair(orientation)/c;f.projection[2][2]=-Pair(orientation)/c;
      f.projection[2][1]=-Pair(orientation)*(u-z[2])/c;}
    return f;
  };
  if(!pair_polish(state,free,{{1,2,0}},tol,maxit,objective,detail))return false;
  const Pair k=free[0]?exponential(state[0]):Pair(shape),c=free[1]?exponential(state[1]):Pair(power);
  const Pair logs=free[2]?logarithm(Pair(unit))+Pair(orientation)*((free[0]?state[0]:logarithm(Pair(shape)))-state[2])/c:logarithm(Pair(unit));
  const double s=free[2]?value(exponential(logs)):unit;
  if(!(s>0)||!std::isfinite(s)||(free[2]&&std::abs(value(logarithm(Pair(s))-logs))>tol*.25))return false;
  shape=value(k);power=value(c);scale=s;return true;
}
inline bool joint_fit(span x,double& shape,double& power,double& scale,
    const std::array<bool,3>& free,double tol,int maxit,bool inverse=false,double shift=0) {
  StopDetail detail;return joint_fit(x,shape,power,scale,free,tol,maxit,inverse,shift,detail);
}

}}
