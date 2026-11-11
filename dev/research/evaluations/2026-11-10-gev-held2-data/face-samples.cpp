#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <iomanip>
#include <iostream>
#include <random>
#include <vector>
int main() {
  std::mt19937_64 rng(17020261110ULL);std::uniform_real_distribution<double> uniform(0,1);int id=0;
  std::cout<<"sample_id,a,b,c\n"<<std::setprecision(17);
  for(double shape:{-.9,-.5,0.,.5,1.,2.})for(int n:{3,10,50,100,500})for(int rep=0;rep<2;++rep) {
    const int sample=id++;std::vector<double> x;
    for(int j=0;j<n;++j)x.push_back(cppunivariate::distributions::qgev(uniform(rng),0,1,shape,true,false));
    if(sample==10||sample==20||sample==60||sample==80)std::cout<<sample<<','<<x[0]<<','<<x[1]<<','<<x[2]<<'\n';
    if(shape>=0) {++id;for(int j=0;j<n;++j)uniform(rng);}
  }
}
