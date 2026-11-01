// Seeded status comparison: compile once against each source revision.
// See the 2026-11-01 existence-fu report for commands and limitations.
#include "cppunivariate/distributions/nbinom/mle.hpp"
#include <random>
#include <iostream>
#include <vector>
int main() {
  std::mt19937 rng(20261101);
  for (int j=0;j<1000;++j) {
    std::vector<int> x;
    std::negative_binomial_distribution<int> d(1+j%20, .1 + .8*(j%41)/40.0);
    for (int i=0;i<50+j%151;++i) x.push_back(d(rng));
    auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1.220703e-4,100);
    std::cout << static_cast<int>(r.status) << '\n';
  }
  // Two observations have mean k^2-1 and population variance k^2:
  // an exact variance excess of one, increasingly small relative to the mean.
  for (int k=2;k<=46339;++k) {
    const int m=k*k-1;
    std::vector<int> x{m-k,m+k};
    auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1.220703e-4,1);
    std::cout << static_cast<int>(r.status) << '\n';
  }
  for (int j=10;j<=10000;j*=10) {
    std::vector<int> x(j,0); for (int i=1;i<j;i+=2) x[i]=2; x[1]=3;
    auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1.220703e-4,1);
    std::cout << static_cast<int>(r.status) << '\n';
  }
}
