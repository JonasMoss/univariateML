#include "cppunivariate/distributions/tlambda/mle.hpp"
#include <chrono>
#include <cstdio>
#include <cstdlib>
using namespace cppunivariate;
static double now(){return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();}
int main(int argc,char** argv){
  const int n=std::atoi(argv[1]); const double l0=std::atof(argv[2]); const int maskmax=argc>3?std::atoi(argv[3]):7;
  std::vector<double> x(n);
  for(int i=0;i<n;++i) x[i]=distributions::qtlambda((i+.5)/n,0,1,l0);
  span s{x.data(),x.size()};
  // per-density cost
  {
    volatile double acc=0; const int reps=std::max(1,200000/n);
    double t0=now(); for(int r=0;r<reps;++r) for(double v:x) acc+=distributions::dtlambda(v,0.01,1.03,l0,true);
    double t1=now(); std::printf("n=%d l0=%g  density: %.1f ns/obs\n",n,l0,1e9*(t1-t0)/(double(reps)*n));
  }
  for(int mask=0;mask<=maskmax;++mask){
    if(mask==7) continue;
    const double mu=(mask&1)?0.0:NAN, sg=(mask&2)?1.0:NAN, la=(mask&4)?l0:NAN;
    double t0=now(); auto r=tlambda_mle(s,1e-7,1000,la,mu,sg); double t1=now();
    std::printf("  mask %d: %8.4f s  status=%d cert=%d it=%d  mu=%.10g sigma=%.10g lambda=%.10g ll=%.12g\n",
      mask,t1-t0,(int)r.status,(int)r.certified,r.iterations,r.mu,r.sigma,r.lambda,r.logLik);
  }
}
