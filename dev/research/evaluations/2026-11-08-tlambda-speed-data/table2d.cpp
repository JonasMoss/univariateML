#include "table2d.hpp"
#include "cppunivariate/distributions/tlambda/dpqr.hpp"
#include <chrono>
#include <cstdio>
#include <cstdlib>
static double now(){return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();}
int main(int argc,char**argv){
  const int NL=std::atoi(argv[1]),NT=std::atoi(argv[2]); const double c=std::atof(argv[3]);
  double t0=now(); const auto T=t2d::build(NL,NT,c); double t1=now();
  // validation: off-grid lambdas, eta from 1e-6 to far tail / endpoint
  double maxerr=0, wl=0, we=0; long cnt=0; t2d::steps=0;
  for(int i=0;i<=200;++i){ const double l=-1+2.0*(i+0.37)/201; if(l>=1) continue;
    for(double le=-6;le<=std::log10(l>0?700/l:700.0);le+=0.01){ const double eta=std::pow(10.0,le);
      double az,ld; t2d::z_of_eta(eta,l,az,ld); if(!(az>0)||!std::isfinite(az)) continue;
      const double ex=t2d::exact(eta,l), g=t2d::guess(T,eta,l);
      const double e=std::abs(g-ex)/(1+std::abs(ex)); ++cnt;
      if(e>maxerr){maxerr=e;wl=l;we=eta;} } }
  // step count and speed for the full density path: eta from z, guess, one Halley, D
  long n=0; t2d::steps=0; volatile double acc=0;
  std::vector<std::pair<double,double>> q;
  for(int i=0;i<=40;++i){ const double l=-1+2.0*(i+0.37)/41; for(int k=0;k<4000;++k){ double z=std::abs(cppunivariate::distributions::qtlambda((k+.5)/4000,0,1.07,l)); if(z>0 && (l<=0 || l*z<1)) q.push_back({z,l});}}
  double t2=now();
  for(auto& [z,l]:q){ double ld; const double eta=t2d::eta_of(z,l,ld); double D; t2d::halley(z,ld,l,t2d::guess(T,eta,l),D); acc+=-std::log(D); ++n; }
  double t3=now(); const long st=t2d::steps;
  for(auto& [z,l]:q) acc+=cppunivariate::distributions::dtlambda(z,0,1,l,true);
  double t4=now();
  double maxd=0; for(auto& [z,l]:q){ double ld; const double eta=t2d::eta_of(z,l,ld); double D; t2d::halley(z,ld,l,t2d::guess(T,eta,l),D);
    maxd=std::max(maxd,std::abs(-std::log(D)-cppunivariate::distributions::dtlambda(z,0,1,l,true))/(1+std::abs(std::log(D)))); }
  std::printf("NL=%d NT=%d c=%.2f  table %zu doubles (%.1f KB), build %.1f ms | max guess err %.2e (l=%.3f eta=%.3g) over %ld pts | Halley steps/obs %.3f | %.0f ns vs dtlambda %.0f ns | max |diff| vs dtlambda %.1e\n",
    NL,NT,c,T.r.size(),T.r.size()*8/1024.0,1e3*(t1-t0),maxerr,wl,we,cnt,double(st)/n,1e9*(t3-t2)/n,1e9*(t4-t3)/n,maxd);
}
