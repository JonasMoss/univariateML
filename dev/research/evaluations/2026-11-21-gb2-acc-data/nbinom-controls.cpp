#include "cppunivariate/distributions/nbinom/mle.hpp"
#include <iomanip>
#include <iostream>
#include <vector>

void check(const std::vector<int>& x,double tol,int maxit) {
  const cppunivariate::ispan sample{x.data(),x.size()};
  const auto proposal=cppunivariate::nbinom_candidate_mle(sample,tol,maxit);
  const auto fit=cppunivariate::nbinom_mle(sample,tol,maxit);
  std::cout<<std::setprecision(17)<<tol<<' '<<static_cast<int>(proposal.status)
      <<' '<<proposal.size<<' '<<static_cast<int>(fit.status)<<' '<<fit.size
      <<' '<<fit.prob<<' '<<fit.iterations<<'\n';
}
int main() {
  std::vector<int> sample;
  for(int i=0;i<999;++i)for(int j=0;j<5;++j)sample.push_back(j);
  sample.push_back(0);
  sample.push_back(4);
  for(double tol:{1e-6,1e-10,1e-13})check(sample,tol,200);
  check({0,0,1,3,6},1e-12,1);
  check({9000,11000},1e-12,200);
}
