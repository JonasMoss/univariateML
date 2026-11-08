#include "cppunivariate/distributions/zib/mle.hpp"
#include <iostream>
#include <vector>
int main() {
  std::vector<int>x(10000,0);
  x.insert(x.end(),4937,1);x.insert(x.end(),1126,2);
  x.insert(x.end(),2937,3);x.insert(x.end(),1000,4);
  const auto fit=cppunivariate::zib_mle({x.data(),x.size()},1e-10,80);
  std::cout<<"status="<<int(fit.status)<<" budget="<<fit.stop_detail.budget_exhausted
    <<" comparison="<<fit.stop_detail.comparison_tie<<" upper="<<fit.stop_detail.loglik_upper
    <<" candidates="<<fit.stop_detail.candidates.size()<<"\n";
  return fit.status==cppunivariate::ZibStatus::IterationFailure && fit.stop_detail.budget_exhausted?0:1;
}
