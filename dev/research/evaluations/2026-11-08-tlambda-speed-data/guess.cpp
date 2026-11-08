#include "cppunivariate/distributions/tlambda/dpqr.hpp"
#include <chrono>
#include <cstdio>
#include <vector>
using namespace cppunivariate; using namespace cppunivariate::tlambda_detail;
static double now(){return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();}
static long newton_steps=0;
// t = -log(p), p the smaller tail, for |z| = h(p) = ((1-p)^l - p^l)/l, non-deficit branch.
// Initial guess: exact at z=0 and asymptotically exact in the tail, then one fixed-point pass
// p <- ((1-p)^l - l|z|)^(1/l) written in logs, then Newton in t as in dpqr.hpp.
inline double guess_t(double az,double l){
  if(l==0) return az+std::log1p(std::exp(-az));
  if(l<0){
    const double a=-l;
    double t=std::log(std::fma(a,az,std::exp(a*std::log(2.0))))/a;         // p0=(a|z|+2^a)^(-1/a)
    const double lv=std::log1p(-std::exp(-t));
    return std::log(std::fma(a,az,std::exp(-a*lv)))/a;                     // fixed-point pass
  }
  // 0<l: p^l = (1-p)^l - l|z|
  double t=-std::log(std::max(1e-300,std::exp(-l*std::log(2.0))-l*az))/l;  // p0 from (1-p)^l ~ 2^-l
  t=std::max(t,std::log(2.0));
  const double lv=std::log1p(-std::exp(-t));
  const double r=std::exp(l*lv)-l*az;
  if(r>0) t=std::max(std::log(2.0),-std::log(r)/l);
  return t;
}
inline double fast_logsmall(double target,double l){
  const double az=std::exp(target);
  if(l==0) return -guess_t(az,0);
  double t=guess_t(az,l);
  for(int k=0;k<100;++k){
    ++newton_steps;
    const double lv=std::log1p(-std::exp(-t));
    const double m=logmag_from_logs(-t,lv,l), f=m-target;
    if(std::abs(f)<2e-14) break;
    const double slope=std::exp(logderiv_from_logs(-t,lv,l)-t-m);
    const double next=std::max(std::log(2.0),t-f/slope);
    if(std::abs(next-t)<2e-14*(1+t)){t=next;break;}
    t=next;
  }
  return -t;
}
int main(){
  for(double l:{-1.0,-0.7,-0.5,-0.2,-0.05,0.05,0.14,0.3,0.5,0.7,0.9}){
    const int n=20001; std::vector<double> tg;
    for(int i=0;i<n;++i){double z=distributions::qtlambda((i+.5)/n,0,1.07,l);
      if(z==0) continue; if(l>0 && std::log(l)+std::log(std::abs(z))> -std::log(2.0)) continue; tg.push_back(std::log(std::abs(z)));}
    // add far tails for l<=0
    if(l<=0) for(double e=1;e<300;e*=1.5) tg.push_back(e);
    newton_steps=0; double maxrel=0; volatile double acc=0;
    double t0=now(); for(double g:tg) acc+=logsmall_log(g,l); double t1=now();
    for(double g:tg) acc+=fast_logsmall(g,l); double t2=now();
    for(double g:tg){double a=logsmall_log(g,l),b=fast_logsmall(g,l); maxrel=std::max(maxrel,std::abs(a-b)/std::abs(a));}
    std::printf("l=%5.2f  m=%zu  current %.0f ns  guess+newton %.0f ns  steps/obs %.2f  max rel diff in log p %.2e\n",
      l,tg.size(),1e9*(t1-t0)/tg.size(),1e9*(t2-t1)/tg.size(),double(newton_steps)/(2*tg.size()),maxrel);
  }
}
