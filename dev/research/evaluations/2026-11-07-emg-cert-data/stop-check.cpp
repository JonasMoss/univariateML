#include "cppunivariate/distributions/emg/mle.hpp"
#include <iostream>
int main() {
  const std::vector<double> x{-2,-1,0,1,2},tiny{1,2};
  for(int budget:{1,2,500}) {
    const auto r=cppunivariate::emg_mle({x.data(),x.size()},1e-10,budget,0,1);
    std::cout<<budget<<','<<int(r.status)<<','<<r.iterations<<','<<r.stop_detail.candidates.size()<<'\n';
  }
  const auto r=cppunivariate::emg_mle({tiny.data(),tiny.size()},1e-8,500,0,1e-10);
  std::cout<<"tiny,"<<int(r.status)<<','<<r.iterations<<','<<r.lambda<<'\n';
  return r.status==cppunivariate::EmgStatus::Ok?0:1;
}
