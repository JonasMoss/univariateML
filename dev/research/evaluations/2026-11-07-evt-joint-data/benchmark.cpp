#include "cppunivariate/distributions/frechet/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include "evt-joint-golden.hpp"
int main() {
  const std::vector<double> ordinary{-.7,-.2,0,.4,.8,1.5,3}, half{0,0,1,2};
  const std::vector<double> ordinary_frechet{0,.2,.5,1,3,20,100};
  std::vector<double> two_point(10001,1);two_point[0]=0;
  std::cout<<std::setprecision(17);
  for(int job=0;job<9;++job) {
    const auto start=std::chrono::steady_clock::now();double sum=0;
    const int repeats=(job==7 || job==8)?3:100;
    for(int j=0;j<repeats;++j) {
      if(job<3 || job==5 || job==7 || job==8) {
        const auto& x=job==8?two_point:(job==5 || job==7)?gev_golden:job==2?half:ordinary;
        const auto f=cppunivariate::gev_mle({x.data(),x.size()},1e-10,100,NAN,job==7?gev_mu:job==1?.1:NAN,job==7?gev_sigma:job==1?1.3:NAN);
        sum+=std::isfinite(f.logLik)?f.logLik:0;
        if(j==0)std::cout<<job<<" "<<int(f.status)<<" "<<f.mu<<" "<<f.sigma<<" "<<f.xi<<" "<<f.logLik<<" ";
      } else {
        const auto& x=job==6?frechet_golden:job==4?half:ordinary_frechet;
        const auto f=cppunivariate::frechet_mle({x.data(),x.size()});
        sum+=std::isfinite(f.logLik)?f.logLik:0;
        if(j==0)std::cout<<job<<" "<<int(f.status)<<" "<<f.lambda<<" "<<f.mu<<" "<<f.sigma<<" "<<f.logLik<<" ";
      }
    }
    std::cout<<std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-start).count()/repeats<<" "<<sum<<"\n";
  }
}
