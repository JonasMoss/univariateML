#include "cppunivariate/distributions/tlambda/dpqr.hpp"
#include <cstdio>
#include <cstdlib>
using namespace cppunivariate; using namespace cppunivariate::tlambda_detail;
// copy of the general (non-deficit) branch of logsmall_log, instrumented
static int nb=0,nn=0;
double inv_general(double target,double lambda){
  double lo=std::log(2.0),hi=2;
  while(logmag(-hi,lambda)<target){hi*=2;++nb;}
  double t=lo+(hi-lo)/2;
  for(int k=0;k<100;++k){
    ++nn;
    const double lv=std::log1p(-std::exp(-t));
    const double magnitude=logmag_from_logs(-t,lv,lambda);
    const double f=magnitude-target;
    if(std::abs(f)<2e-14) break;
    if(f>0) hi=t; else lo=t;
    const double slope=std::exp(logderiv_from_logs(-t,lv,lambda)-t-magnitude);
    double next=t-f/slope;
    if(!(next>lo && next<hi) || !std::isfinite(next)) next=lo+(hi-lo)/2;
    if(std::abs(next-t)<2e-14*(1+t)) {t=next;break;}
    t=next;
  }
  return -t;
}
int main(){
  for(double l:{-1.0,-0.5,-0.2,0.14,0.5,0.9}){
    const int n=1001; nb=nn=0; int used=0;
    for(int i=0;i<n;++i){
      double z=distributions::qtlambda((i+.5)/n,0,1.03,l)/1.0;   // slightly misspecified scale
      if(z==0) continue;
      if(l>0 && std::log(l)+std::log(std::abs(z))> -std::log(2.0)) continue; // deficit branch, skip
      inv_general(std::log(std::abs(z)),l); ++used;
    }
    std::printf("lambda=%5.2f  used=%d  bracket doublings/obs=%.2f  newton/obs=%.2f\n",l,used,double(nb)/used,double(nn)/used);
  }
}
