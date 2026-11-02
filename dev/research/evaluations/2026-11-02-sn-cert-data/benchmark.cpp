#ifdef OLD_SN
#include "sn_old.hpp"
#else
#include "cppunivariate/distributions/sn/mle.hpp"
#endif
#include <vector>
#include <cstdio>
#include <chrono>
int main() {
  std::vector<double> x;double v;while(std::scanf("%lf",&v)==1)x.push_back(v);
  const auto t=std::chrono::steady_clock::now();
  auto f=cppunivariate::sn_mle(cppunivariate::span{x.data(),x.size()});
  std::printf("n=%zu status=%d xi=%.17g omega=%.17g alpha=%.17g ll=%.17g evals=%d seconds=%.6f\n",x.size(),int(f.status),f.xi,f.omega,f.alpha,f.logLik,f.iterations,std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count());
}
