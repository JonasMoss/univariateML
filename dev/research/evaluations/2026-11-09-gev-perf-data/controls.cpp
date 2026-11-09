#include "cppunivariate/distributions/frechet/mle.hpp"
#include "evt-joint-golden.hpp"
#include <iostream>
int main() {
  for(double tol:{1e-10,1e-16})for(int cap:{1,2,5,10,20,50,100})for(int job:{0,1}) {
    const auto& x=job==0?gev_golden:frechet_golden;
    cppunivariate::StopDetail detail;int status;unsigned restricted;
    if(job==0) {
      const auto f=cppunivariate::gev_mle({x.data(),x.size()},tol,cap);
      detail=f.stop_detail;status=int(f.status);restricted=f.restricted;
    } else {
      const auto f=cppunivariate::frechet_mle({x.data(),x.size()},tol,cap);
      detail=f.stop_detail;status=int(f.status);restricted=f.restricted;
    }
    std::cout<<job<<" "<<tol<<" "<<cap<<" "<<status<<" "<<restricted<<" "
      <<detail.budget_exhausted<<" "<<detail.comparison_tie<<" "<<int(detail.limit_model)<<"\n";
  }
}
