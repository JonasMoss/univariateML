#include "cppunivariate/distributions/std/mle.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main(){std::cout<<std::setprecision(17);
  for(int sample=0;sample<5;++sample){
    std::vector<double>x;
    if(sample==0)x={0,0,1,2,10};
    if(sample==1)x={-100,-2,-.01,0,.01,2,100};
    if(sample==2)for(int i=0;i<4000;++i)x.push_back(cppunivariate::special::qt((i+.5)/4000,300,true,false));
    if(sample==3||sample==4){for(int i=0;i<1000;++i)x.push_back(cppunivariate::special::qt((i+.5)/1000,4,true,false)/std::sqrt(2.));if(sample==4){auto base=x;for(int j=1;j<100;++j)x.insert(x.end(),base.begin(),base.end());}}
    for(double floor:{1.,2.}){auto start=std::chrono::steady_clock::now();auto r=cppunivariate::t_certificate({x.data(),x.size()},floor,4096,500);std::cout<<sample<<" "<<floor<<" "<<int(r.status)<<" "<<r.mu<<" "<<r.scale<<" "<<r.nu<<" "<<r.logLik<<" "<<r.profiles<<" "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<std::endl;}
  }
}
