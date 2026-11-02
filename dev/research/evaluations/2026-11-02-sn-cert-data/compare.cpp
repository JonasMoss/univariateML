#ifdef OLD_SN
#include "sn_old.hpp"
#else
#include "cppunivariate/distributions/sn/mle.hpp"
#endif
#include <random>
#include <vector>
#include <cstdio>
int main() {
 std::mt19937 rng(20261102);std::normal_distribution<double> z;
 const double shapes[]={-100,-20,-5,-1,-.001,0,.001,1,5,20,100,1e8};
 for(int j=0;j<600;++j) {
  double a=shapes[j%12];std::vector<double> x;
  for(int i=0;i<100;++i)x.push_back((a*std::abs(z(rng))+z(rng))/std::hypot(1.,a));
  auto f=cppunivariate::sn_mle(cppunivariate::span{x.data(),x.size()});
  std::printf("%d %d %.17g %.17g",j,int(f.status),f.alpha,f.logLik);
#ifdef EMIT_DATA
  for(double y:x) std::printf(" %.17g",y);
#endif
  std::printf("\n");
 }
}
