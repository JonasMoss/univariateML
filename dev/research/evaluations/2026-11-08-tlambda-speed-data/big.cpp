#include "proto.hpp"
#include <chrono>
#include <cstdio>
#include <random>
static double now(){return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count();}
int main(int argc,char**argv){
  const int n=std::atoi(argv[1]);
  std::vector<double> x(n); for(int i=0;i<n;++i) x[i]=cppunivariate::distributions::qtlambda((i+.5)/n,0,1,.14);
  double t0=now(); auto P=proto::fit(x); double t1=now();
  std::printf("n=%d proto %.2fs passes=%ld lambda=%.12f\n",n,t1-t0,P.passes,P.lambda);
}
