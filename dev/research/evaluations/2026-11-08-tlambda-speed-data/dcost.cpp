#include "cppunivariate/distributions/tlambda/dpqr.hpp"
#include <chrono>
#include <cstdio>
#include <vector>
using namespace cppunivariate; using namespace cppunivariate::tlambda_detail;
static double now(){return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();}
int main(){
  for(double l:{-0.5,0.14,0.7}){
    const int n=101; std::vector<double> x(n); for(int i=0;i<n;++i) x[i]=distributions::qtlambda((i+.5)/n,0,1,l);
    const int reps=20000; volatile double acc=0;
    double t0=now(); for(int r=0;r<reps;++r) for(double v:x) acc+=distributions::dtlambda(v,0.01,1.03,l,true);
    double t1=now(); for(int r=0;r<reps;++r) for(double v:x) acc+=logsmall_log(logstandard(v,0.01,1.03),l,l>0?std::log(slack(v,0.01,1.03,l))-std::log(1.03):NAN);
    double t2=now(); for(int r=0;r<reps;++r) for(double v:x) acc+=logsmall_log(logstandard(v,0.01,1.03),l);
    double t3=now(); for(int r=0;r<reps;++r) for(double v:x) acc+=logstandard(v,0.01,1.03)+(l>0?slack(v,0.01,1.03,l):0);
    double t4=now();
    const double m=double(reps)*n;
    std::printf("l=%5.2f dtlambda %.0f ns | inversion w/ slack arg %.0f | inversion, no slack %.0f | logstandard+slack %.0f\n",l,1e9*(t1-t0)/m,1e9*(t2-t1)/m,1e9*(t3-t2)/m,1e9*(t4-t3)/m);
  }
}
