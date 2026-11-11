#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/gev/mle.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <iomanip>
#include <iostream>
#include <random>
int main() {
  std::mt19937_64 rng(17020261110ULL);
  std::uniform_real_distribution<double> uniform(0,1);
  int id=0;
  std::cout<<"sample_id,hold,status,certified,mu,sigma,xi,loglik\n"<<std::setprecision(17);
  for(double shape:{-.9,-.5,0.,.5,1.,2.})for(int n:{3,10,50,100,500})for(int rep=0;rep<2;++rep) {
    const int sample=id++;
    std::vector<double> x;
    for(int j=0;j<n;++j)x.push_back(cppunivariate::distributions::qgev(uniform(rng),0,1,shape,true,false));
    if(sample==8||sample==10||sample==20||sample==60||sample==80||sample==96) {
      const bool scale=sample==8;
      const auto r=cppunivariate::gev_mle({x.data(),x.size()},1e-10,100,NAN,scale?NAN:0,scale?1:NAN);
      std::cout<<sample<<','<<(scale?"sigma":"mu")<<','<<int(r.status)<<','<<r.certified<<','<<r.mu<<','<<r.sigma<<','<<r.xi<<','<<r.logLik<<'\n';
    }
    if(shape>=0) {
      ++id;
      // Consume the same Frechet uniforms as the unchanged held study.
      for(int j=0;j<n;++j)uniform(rng);
    }
  }
}
