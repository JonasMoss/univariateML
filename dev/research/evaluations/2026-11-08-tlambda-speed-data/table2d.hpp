// Prototype: one static 2-D table r(eta, lambda) = log p + eta for lambda in [-1,1],
// eta = log(1 - lambda|z|)/(-lambda). Query: local 4x4 cubic Lagrange in (t, lambda), then Halley in s = log p.
#pragma once
#include <cmath>
#include <vector>
#include <algorithm>
namespace t2d {
static long steps=0;
struct Pow { double p,q,lq,P,R,E,D,Dp; };
inline Pow powers(double s,double l){
  Pow w; w.p=std::exp(s); w.q=1-w.p; w.lq=std::log1p(-w.p);
  w.P=std::exp(l*s); w.E=std::expm1(l*(w.lq-s)); w.R=w.P+w.P*w.E;
  w.D=w.P/w.p+w.R/w.q; w.Dp=(l-1)*(w.P/(w.p*w.p)-w.R/(w.q*w.q));
  return w;
}
// Target given by (az, ld): |z| and, for l>0, log of the deficit 1-l|z| (exact from eta).
// Three forms: centre h(p)=|z|; tail log h = log|z|; deficit log d = ld (l>0 near the endpoint).
inline double halley(double az,double ld,double l,double s,double& D,double tol=1e-5,int maxit=60){
  const double la=std::log(az);
  const bool deficit=l>0 && ld< -0.7;            // l|z| > ~0.5
  const bool tail=!deficit && az>1;
  for(int k=0;k<maxit;++k){
    ++steps;
    Pow w=powers(s,l);
    double ds;
    if(l==0){ // logistic: exact
      s=-(az+std::log1p(std::exp(-az))); D=1/(std::exp(s)*(-std::expm1(s))); return s; }
    const double h=w.P*w.E/l;
    if(deficit){
      const double d=w.P-std::expm1(l*w.lq);              // p^l + 1 - q^l
      const double G=std::log(d)-ld, G1=w.p*l*w.D/d;
      const double G2=(w.p*l*w.D+w.p*w.p*l*w.Dp)/d-G1*G1;
      ds=-2*G*G1/(2*G1*G1-G*G2);
    } else if(tail){
      const double G=std::log(h)-la, pD=w.p*w.D, G1=-pD/h;
      const double G2=(-pD-w.p*w.p*w.Dp)/h-G1*G1;
      ds=-2*G*G1/(2*G1*G1-G*G2);
    } else {
      const double H=h-az, H1=-w.D, H2=-w.Dp;
      const double dp=-2*H*H1/(2*H1*H1-H*H2);
      ds=std::log1p(std::max(-0.9,dp/w.p));
    }
    double ns=std::min(s+ds,-std::log(2.0)); if(!std::isfinite(ns)) ns=s;
    const bool done=std::abs(ns-s)<=tol*(1+std::abs(s));
    s=ns;
    if(done){ D=powers(s,l).D; return s; }
  }
  D=NAN; return s;
}
inline double eta_of(double az,double l,double& ld){       // also returns log deficit for l>0
  if(l==0){ld=0;return az;}
  if(l<0){ld=0;return std::log1p(-l*az)/(-l);}
  ld=std::log1p(-l*az); return -ld/l;
}
inline void z_of_eta(double eta,double l,double& az,double& ld){
  if(l==0){az=eta;ld=0;return;}
  if(l<0){az=std::expm1(-l*eta)/(-l);ld=0;return;}
  ld=-l*eta; az=-std::expm1(ld)/l;
}
inline double crude(double eta,double l){ return -eta-std::log(2.0)*std::exp(-eta); }
// exact solve for table nodes
inline double exact(double eta,double l){
  if(eta==0) return -std::log(2.0);
  double az,ld; z_of_eta(eta,l,az,ld); double D;
  double s=halley(az,ld,l,crude(eta,l),D,1e-15,200);
  return s;
}
struct Table {
  int NL,NT; double c;             // t = 1 - exp(-c*eta)
  std::vector<double> r;           // r[i*NT+j], lambda_i=-1+2i/(NL-1), t_j=j/(NT-1)
};
inline Table build(int NL,int NT,double c){
  Table T{NL,NT,c,std::vector<double>(size_t(NL)*NT)};
  for(int i=0;i<NL;++i){ const double l=-1+2.0*i/(NL-1);
    for(int j=0;j<NT;++j){ const double t=double(j)/(NT-1);
      double rr;
      if(j==NT-1) rr=(l>=1?-std::log(2.0):0);              // eta=inf limit (l=1: uniform keeps -log 2)
      else { const double eta=-std::log1p(-t)/c; rr=exact(eta,l)+eta; }
      T.r[size_t(i)*NT+j]=rr; } }
  return T;
}
inline void lag4(double x,int n,int& k,double w[4]){        // 4-point Lagrange weights on uniform nodes 0..n-1
  k=std::max(0,std::min(n-4,int(std::floor(x))-1)); const double u=x-k;
  w[0]=-(u-1)*(u-2)*(u-3)/6; w[1]=u*(u-2)*(u-3)/2; w[2]=-u*(u-1)*(u-3)/2; w[3]=u*(u-1)*(u-2)/6;
}
inline double guess(const Table& T,double eta,double l){
  const double t=-std::expm1(-T.c*eta);
  int ki,kj; double wi[4],wj[4];
  lag4((l+1)*(T.NL-1)/2,T.NL,ki,wi); lag4(t*(T.NT-1),T.NT,kj,wj);
  double r=0;
  for(int a=0;a<4;++a){ double row=0; const double* R=&T.r[size_t(ki+a)*T.NT+kj];
    for(int b=0;b<4;++b) row+=wj[b]*R[b]; r+=wi[a]*row; }
  return std::min(-eta+r,-std::log(2.0));
}
}
