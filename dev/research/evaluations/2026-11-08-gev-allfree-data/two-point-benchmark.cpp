#include "cppunivariate/distributions/frechet/mle.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main(){
 for(int k:{1,4,9}) {std::vector<double>x(20,1);std::fill_n(x.begin(),k,0.);auto start=std::chrono::steady_clock::now();cppunivariate::FrechetResult f{};
 for(int j=0;j<100;++j)f=cppunivariate::frechet_mle({x.data(),x.size()});
 std::cout<<std::setprecision(17)<<k<<" "<<int(f.status)<<" "<<f.lambda<<" "<<f.mu<<" "<<f.sigma<<" "<<f.logLik<<" "<<std::chrono::duration<double,std::micro>(std::chrono::steady_clock::now()-start).count()/100<<"\n";
 }
}
