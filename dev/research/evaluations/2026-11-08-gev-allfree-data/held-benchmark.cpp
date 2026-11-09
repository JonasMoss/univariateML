#include "cppunivariate/distributions/frechet/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main(){
 const std::vector<double> g{0,2,2,2};std::vector<double> f(16,1);std::fill_n(f.begin(),7,0.);
 for(int job=0;job<3;++job){const auto start=std::chrono::steady_clock::now();double sum=0;
  for(int j=0;j<100;++j){
   if(job<2){const auto r=cppunivariate::gev_mle({g.data(),g.size()},1e-10,100,NAN,job==0?1.5:NAN,job==1?.5:NAN);sum+=r.logLik;if(j==0)std::cout<<job<<" "<<int(r.status)<<" "<<std::setprecision(17)<<r.mu<<" "<<r.sigma<<" "<<r.xi<<" "<<r.logLik<<" ";}
   else {const auto r=cppunivariate::frechet_mle({f.data(),f.size()},1e-10,100,NAN,NAN,.125);sum+=r.logLik;if(j==0)std::cout<<job<<" "<<int(r.status)<<" "<<std::setprecision(17)<<r.lambda<<" "<<r.mu<<" "<<r.sigma<<" "<<r.logLik<<" ";}
  }
  std::cout<<std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-start).count()/100<<" "<<sum<<"\n";
 }
}
