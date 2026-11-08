// Experiment: faster inverse for Tukey lambda density, non-deficit region (lambda<=0, or lambda>0 with lambda|z|<=1/2).
// Unknown s = log p (p = smaller tail). Per step: exp(s), log1p(-p), exp(l*s), expm1(l*Delta) [+ log in tail form].
// Both forms use Halley (cubic): derivatives are algebraic in the powers already computed.
#include "cppunivariate/distributions/tlambda/dpqr.hpp"
#include <chrono>
#include <cstdio>
#include <vector>
#include <cmath>
using namespace cppunivariate;
static double now(){return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();}
static long steps=0;
struct Pow { double p,q,lq,P,R,E,h,D,Dp; };
inline Pow powers(double s,double l){
  Pow w; w.p=std::exp(s); w.q=1-w.p; w.lq=std::log1p(-w.p);
  const double Dl=w.lq-s; w.P=std::exp(l*s); w.E=std::expm1(l*Dl); w.R=w.P+w.P*w.E;
  w.h=w.P*w.E/l; w.D=w.P/w.p+w.R/w.q; w.Dp=(l-1)*(w.P/(w.p*w.p)-w.R/(w.q*w.q));
  return w;
}
// Halley on G(s)=log h - log|z| (tail) or H(p)=h-|z| (centre); returns log p and final D.
inline double solve(double az,double l,double s,double& Dout){
  const double target=std::log(az); const bool tail=az>1;
  for(int k=0;k<50;++k){
    ++steps;
    Pow w=powers(s,l);
    double ds;
    if(tail){
      const double G=std::log(w.h)-target, pD=w.p*w.D, G1=-pD/w.h;
      const double ph=-pD, pphh=-w.p*w.p*w.Dp;            // p h_p, p^2 h_pp
      const double G2=(ph+pphh)/w.h-G1*G1;
      ds=-2*G*G1/(2*G1*G1-G*G2);
    } else {
      const double H=w.h-az, H1=-w.D, H2=-w.Dp;                // in p
      const double dp=-2*H*H1/(2*H1*H1-H*H2);
      ds=std::log1p(dp/w.p);
    }
    double ns=std::min(s+ds,-std::log(2.0));
    if(!std::isfinite(ns)) ns=s;
    const bool done=std::abs(ns-s)<=1e-5*(1+std::abs(s));   // cubic: next error ~ step^3
    s=ns;
    if(done){ Pow f=powers(s,l); Dout=f.D; return s; }
  }
  Dout=NAN; return s;
}
inline double crude_guess(double az,double l){       // exact at z=0, asymptotically exact tails
  if(l<0){const double a=-l; return -std::log(std::fma(a,az,std::exp(a*std::log(2.0))))/a;}
  const double r=std::exp(-l*std::log(2.0))-l*az; return r>0?std::min(-std::log(2.0),std::log(r)/l):-30;
}
// Per-lambda table: cubic Hermite of s(w), w=log1p(|z|), on [0,W] with exact slopes.
struct Table { double l,W,hw; std::vector<double> s,ds; };
inline Table build(double l,int K=64){
  Table T; T.l=l; T.W=l>0?std::log1p(0.5/l):std::log1p(1e6); T.hw=T.W/(K-1);
  for(int k=0;k<K;++k){
    const double w=k*T.hw, az=std::expm1(w); double D;
    const double s=az==0?-std::log(2.0):solve(az,l,crude_guess(az,l),D);
    T.s.push_back(s);
    // ds/dw = (ds/dz)(dz/dw) = -(1/(p D)) * (1+az)   since dp/dz = -1/D
    T.ds.push_back(az==0?-std::exp(std::log(2.0))*0+(-(1.0)/(0.5*std::exp((l-1)*std::log(0.5))*2)):-(1+az)/(std::exp(s)*D));
  }
  return T;
}
inline double table_guess(const Table& T,double az){
  const double w=std::log1p(az); if(w>=T.W) return crude_guess(az,T.l);
  const double x=w/T.hw; int k=std::min(int(x),int(T.s.size())-2); const double t=x-k, h=T.hw;
  const double t2=t*t,t3=t2*t;
  return (2*t3-3*t2+1)*T.s[k]+(t3-2*t2+t)*h*T.ds[k]+(-2*t3+3*t2)*T.s[k+1]+(t3-t2)*h*T.ds[k+1];
}
int main(){
  for(double l:{-0.7,-0.05,0.14,0.7}){
    const Table T=build(l);
    for(int i=0;i<400;++i){
      double v=std::abs(distributions::qtlambda((i+.5)/400,0,1.07,l)); if(!(v>0) || (l>0 && l*v>0.5)) continue;
      double D; solve(v,l,table_guess(T,v),D);
      std::printf("%.17g %.17g %.17g %.17g\n",l,v,distributions::dtlambda(v,0,1,l,true),-std::log(D));
    }
  }
}
