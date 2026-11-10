// Smallest iterlim (of 100, 200, 400, 800, 1600) at which deterministic quantile
// samples certify with public scale held (TASK-172).
#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
#include <cstdio>
namespace cu=cppunivariate;
int main() {
  for(int n:{500,1000,2000}) for(double lam:{2.,1.25,4.}) {
    std::vector<double> x;for(int i=0;i<n;++i)x.push_back(std::pow(-std::log((i+.5)/n),-1/lam));
    for(int maxit:{100,200,400,800,1600}) {
      const auto r=cu::frechet_mle({x.data(),x.size()},1e-10,maxit,NAN,NAN,1);
      std::printf("n=%d lambda=%g iterlim=%d certified=%d\n",n,lam,maxit,int(r.certified));std::fflush(stdout);
      if(r.certified)break;
    }
  }
}
