// Smallest iterlim (of 100, 200, 400, 800, 1600) at which deterministic quantile
// samples certify with public scale held (TASK-172).
#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
#include <cstdio>
#include <chrono>
#include <ctime>
namespace cu=cppunivariate;
int main() {
  std::puts("n,lambda,iterlim,status,certified,milliseconds,cpu_milliseconds,evaluations");
  for(int n:{500,1000,2000,5000}) for(double lam:{2.,1.25,4.}) {
    std::vector<double> x;for(int i=0;i<n;++i)x.push_back(std::pow(-std::log((i+.5)/n),-1/lam));
    for(int maxit:{100,200,400,800,1600}) {
      const auto cpu=std::clock();
#ifdef UMVL_STUDY_COUNTS
      cu::gev_held_detail::study_evaluations=0;
#endif
      const auto start=std::chrono::steady_clock::now();
      const auto r=cu::frechet_mle({x.data(),x.size()},1e-10,maxit,NAN,NAN,1);
      std::printf("%d,%g,%d,%d,%d,%.6f,%.6f,%lld\n",n,lam,maxit,int(r.status),int(r.certified),std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count(),1000.*(std::clock()-cpu)/CLOCKS_PER_SEC,
#ifdef UMVL_STUDY_COUNTS
        static_cast<long long>(cu::gev_held_detail::study_evaluations)
#else
        -1LL
#endif
      );std::fflush(stdout);
      if(r.certified)break;
    }
  }
}
