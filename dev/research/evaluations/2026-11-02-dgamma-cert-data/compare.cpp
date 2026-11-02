#include "legacy.hpp"
#include <chrono>
#include <cmath>
#include <cstdio>
#include <random>
#include <vector>
int main() {
  std::mt19937_64 rng(11620261103ULL);
  const double shapes[]={0.15,0.35,1,3,10,50};
  const double rates[]={0.25,1,4};
  const int sizes[]={10,30,80};
  std::printf("case,n,shape,rate,old_status,new_status,old_ll,new_ll,old_ms,new_ms\n");
  for(int i=0;i<620;++i) {
    const int n=i<600?sizes[(i/18)%3]:500;
    const double a=i<600?shapes[i%6]:2.0, r=i<600?rates[(i/6)%3]:1.0;
    std::gamma_distribution<double> distribution(a,1/r);
    std::vector<double> x(n); for(double &v:x) v=std::floor(distribution(rng));
    const auto start=std::chrono::steady_clock::now();
    const auto old=cppunivariate::dgamma_legacy_mle({x.data(),x.size()});
    const auto mid=std::chrono::steady_clock::now();
    const auto now=cppunivariate::dgamma_mle({x.data(),x.size()});
    const auto end=std::chrono::steady_clock::now();
    const double oldms=std::chrono::duration<double,std::milli>(mid-start).count();
    const double newms=std::chrono::duration<double,std::milli>(end-mid).count();
    std::printf("%d,%d,%.17g,%.17g,%d,%d,%.17g,%.17g,%.6f,%.6f\n",i,n,a,r,int(old.status),int(now.status),old.logLik,now.logLik,oldms,newms);
    std::fflush(stdout);
  }
}
