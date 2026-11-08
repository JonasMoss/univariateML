// Prototype: profile-likelihood fitter for all-free Tukey lambda on [-1,1].
#pragma once
#include "cppunivariate/distributions/tlambda/mle.hpp"
#include <vector>
#include <array>
#include <cstdio>
#include <cstdlib>
namespace proto {
using namespace cppunivariate;
struct Obs { double lg, psi, dpsi, dl; bool ok; };
static long passes=0;
// standardized z, shape l (l != 1,2). Returns log g, d/dz log g, d2/dz2 log g, d/dl log g.
inline Obs eval(double z,double l){
  Obs o{0,0,0,0,true};
  const double az=std::abs(z);
  if(l>0 && az*l>=1){o.ok=false;return o;}
  double lp;                                  // log of smaller tail p
  if(az==0) lp=-std::log(2.0);
  else lp=tlambda_detail::logsmall_log(std::log(az),l, l>0? std::log(-std::expm1(std::log(l)+std::log(az))) : NAN);
  const double lq=std::log1p(-std::exp(lp));
  // for z>0, u=q (large), v=p; for z<0 mirror. Work with z>0 and flip odd quantities.
  const double sgn=z<0?-1:1;
  const double Lu=lq, Lv=lp;
  const double um1=std::exp((l-1)*Lu), vm1=std::exp((l-1)*Lv);
  const double D=um1+vm1;
  const double Du=(l-1)*(std::exp((l-2)*Lu)-std::exp((l-2)*Lv));
  const double Duu=(l-1)*(l-2)*(std::exp((l-3)*Lu)+std::exp((l-3)*Lv));
  const double Dl=um1*Lu+vm1*Lv;
  double Ql;
  const double big=std::max(std::abs(Lu),std::abs(Lv))*std::abs(l);
  if(big>0.5) Ql=(std::exp(l*Lu)*Lu-std::exp(l*Lv)*Lv)/l-(std::exp(l*Lu)-std::exp(l*Lv))/(l*l);
  else { // sum_{k>=2} (k-1) l^(k-2) (Lu^k - Lv^k)/k!
    Ql=0; double pu=Lu*Lu, pv=Lv*Lv, fact=2, lk=1;
    for(int k=2;k<60;++k){ const double term=(k-1)*lk*(pu-pv)/fact; Ql+=term; if(std::abs(term)<1e-18*std::abs(Ql)&&k>4)break; pu*=Lu;pv*=Lv;fact*=k+1;lk*=l; }
  }
  o.lg=-std::log(D);
  o.psi=sgn*(-Du/(D*D));
  o.dpsi=-(Duu*D-2*Du*Du)/(D*D*D*D);
  o.dl=-Dl/D+Du*Ql/(D*D);   // Q_l odd in (u-1/2) and D_u odd: product even -> no sign flip
  return o;
}
struct Val { double ll, gA,gB, hAA,hAB,hBB, gl; bool ok; };
inline Val value(const std::vector<double>& x,double A,double B,double l){
  ++passes;
  Val v{0,0,0,0,0,0,0,true};
  const double n=x.size();
  if(!(A>0)){v.ok=false;return v;}
  v.ll=n*std::log(A); v.gA=n/A; v.hAA=-n/(A*A);
  for(double xi:x){
    const auto o=eval(A*xi+B,l);
    if(!o.ok){v.ok=false;return v;}
    v.ll+=o.lg; v.gA+=xi*o.psi; v.gB+=o.psi; v.hAA+=xi*xi*o.dpsi; v.hAB+=xi*o.dpsi; v.hBB+=o.dpsi; v.gl+=o.dl;
  }
  v.ok=std::isfinite(v.ll+v.gA+v.gB+v.hAA+v.hAB+v.hBB+v.gl);
  return v;
}
// Damped Newton (Levenberg fallback) in (A,B) at fixed l.
struct Inner { double A,B; Val v; bool ok; int it; };
static long inner_calls=0, inner_iters=0;
inline Inner inner(const std::vector<double>& x,double A,double B,double l,int maxit=100){
  ++inner_calls;
  if(l>0){ // keep the centre, widen the support to contain the data
    const double mu=-B/A, r=std::max(std::abs(x.front()-mu),std::abs(x.back()-mu));
    const double Amax=1/(1.02*l*r); if(A>Amax){A=Amax;B=-mu*A;}
  }
  Val v=value(x,A,B,l);
  for(int j=0;j<60 && !v.ok;++j){const double mu=-B/A;A*=0.7;B=-mu*A;v=value(x,A,B,l);}
  if(!v.ok) return {A,B,v,false,0};
  double damp=0;
  for(int it=0;it<maxit;++it){
    ++inner_iters;
    const double scale=std::abs(v.hAA)+std::abs(v.hBB);
    double a=-v.hAA+damp, b=-v.hAB, c=-v.hBB+damp, det=a*c-b*b;
    for(int g=0;!(a>0 && c>0 && det>0);++g){
      if(g>2000) return {A,B,v,false,it};
      damp=std::max(2*damp,1e-10*scale+1e-300); a=-v.hAA+damp; c=-v.hBB+damp; det=a*c-b*b;
      if(!std::isfinite(damp)) return {A,B,v,false,it};
    }
    const double dA=( c*v.gA-b*v.gB)/det, dB=(-b*v.gA+a*v.gB)/det;
    const double rel=std::max(std::abs(dA)/A,std::abs(dB)/(std::abs(B)+A));
    double step=1; bool acc=false; Val w;
    const double noise=1e-13*(std::abs(v.ll)+x.size());
    for(int k=0;k<40;++k){
      const double nA=A+step*dA,nB=B+step*dB;
      if(nA>0){ w=value(x,nA,nB,l); if(w.ok && w.ll>=v.ll-noise){A=nA;B=nB;acc=true;break;} }
      step/=2;
    }
    if(!acc){ if(rel<1e-8) return {A,B,v,true,it}; damp=std::max(10*damp,1e-6*scale); continue; }
    v=w;
    if(damp==0 && step==1 && rel<1e-8) return {A,B,v,true,it};   // quadratic phase: next step ~ rel^2
    damp=(step==1)?damp/10:damp; if(damp<1e-12*scale) damp=0;
  }
  return {A,B,v,false,maxit};
}
struct Fit { double mu,sigma,lambda,ll; bool ok; long passes; };
inline Fit fit(std::vector<double> x){
  passes=0;
  std::sort(x.begin(),x.end());
  const double origin=x.front(),range=x.back()-x.front();
  std::vector<double> y(x.size()); for(size_t i=0;i<x.size();++i) y[i]=(x[i]-origin)/range;
  const size_t n=y.size();
  const double med=y[n/2], iqr=y[3*n/4]-y[n/4];
  const std::vector<double> grid{-1,-.75,-.5,-.25,0,.14,.3,.5,.7,.9,.99};
  struct G { double l; Inner r; };
  std::vector<G> prof;
  double wA=NAN,wB=NAN;
  for(double l:grid){
    const double q3=-distributions::qtlambda(.25,0,1,l);
    double s=std::max(iqr/(2*q3),1e-4);
    std::vector<std::array<double,2>> starts;
    auto add=[&](double c){double sc=s; if(l>0) sc=std::max(sc,1.05*l*std::max(c,1-c)); starts.push_back({1/sc,-c/sc});};
    add(med); if(l<0){add(y[n/4]);add(y[3*n/4]);}
    if(std::isfinite(wA)) starts.push_back({wA,wB});
    Inner best{0,0,{},false,0}; best.v.ll=-INFINITY;
    for(auto st:starts){ auto r=inner(y,st[0],st[1],l); if(r.ok && r.v.ll>best.v.ll) best=r; }
    prof.push_back({l,best});
    if(std::getenv("PROTO_DEBUG")) std::fprintf(stderr,"grid l=%5.2f ok=%d ll=%.8f dl=%.4g it=%d\n",l,best.ok,best.v.ll,best.v.gl,best.it);
    if(best.ok){wA=best.A;wB=best.B;}
  }
  // candidates: interior sign changes of profile derivative, and endpoint -1
  Fit out{NAN,NAN,NAN,-INFINITY,false,0};
  auto take=[&](double l,const Inner& r){ if(r.ok && r.v.ll>out.ll){out={(-r.B/r.A),1/r.A,l,r.v.ll,true,0};} };
  for(auto& g:prof) take(g.l,g.r);
  for(size_t j=0;j+1<prof.size();++j){
    auto a=prof[j],b=prof[j+1];
    if(!(a.r.ok&&b.r.ok)) continue;
    if(a.r.v.gl>0 && b.r.v.gl<0){
      double la=a.l,lb=b.l,fa=a.r.v.gl,fb=b.r.v.gl; Inner ra=a.r,rb=b.r; int side=0;
      for(int it=0;it<60;++it){
        double lc=lb-fb*(lb-la)/(fb-fa);              // Illinois
        if(!(lc>la&&lc<lb)) lc=(la+lb)/2;
        const Inner& w=(std::abs(lc-la)<std::abs(lc-lb))?ra:rb;
        Inner rc=inner(y,w.A,w.B,lc);
        if(std::getenv("PROTO_DEBUG")) std::fprintf(stderr,"  root l=%.10f ok=%d ll=%.10f dl=%.4g it=%d A=%g B=%g\n",lc,rc.ok,rc.v.ll,rc.v.gl,rc.it,rc.A,rc.B);
        if(!rc.ok) break;
        take(lc,rc);
        const double fc=rc.v.gl;
        if(std::abs(lb-la)<1e-12*(1+std::abs(lc)) || fc==0) break;
        if((fc>0)){ la=lc;fa=fc;ra=rc; if(side==-1) fb/=2; side=-1; } else { lb=lc;fb=fc;rb=rc; if(side==1) fa/=2; side=1; }
      }
    }
  }
  // endpoint face at l=-1 is already in the grid; uniform member at l=1:
  out.mu=origin+range*out.mu; out.sigma*=range;
  out.ll-=n*std::log(range);
  const auto u=tlambda_detail::uniform_fit(x.front(),x.back(),double(n),1,1000,1e-7);
  if(u.status==TlambdaStatus::Ok && u.logLik>out.ll+1e-8*n){out={u.mu,u.sigma,1,u.logLik,true,0};}
  out.passes=passes;
  std::fprintf(stderr,"");
  return out;
}
}
