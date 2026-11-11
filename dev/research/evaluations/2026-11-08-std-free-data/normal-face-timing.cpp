#include "cppunivariate/distributions/std/mle.hpp"
#include "cppunivariate/distributions/norm/dpqr.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main(){
  std::cout<<std::setprecision(17)<<"family,n,held_mask,repeat,status,certified,seconds\n";
  for(int n:{100,1000}){
    std::vector<double> x;for(int j=0;j<n;++j)x.push_back(cppunivariate::distributions::qnorm((j+.5)/n,0,1,true,false));
    for(bool st:{false,true})for(int mask:{1,2,3})for(int rep=0;rep<3;++rep){
      const double m=mask&1?0:NAN,s=mask&2?1:NAN;
      const auto begin=std::chrono::steady_clock::now();int status=0,certified=-1;
      if(st){
        const auto r=cppunivariate::student_t_mle({x.data(),x.size()},1e-10,200,m,s,NAN);status=static_cast<int>(r.status);
#ifndef UMVL_SIGNED_BASELINE
        certified=r.certified;
#endif
      }else{
        const auto r=cppunivariate::lst_mle({x.data(),x.size()},1e-10,200,m,s,NAN);status=static_cast<int>(r.status);
#ifndef UMVL_SIGNED_BASELINE
        certified=r.certified;
#endif
      }
      const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
      std::cout<<(st?"std":"lst")<<","<<n<<","<<mask<<","<<rep<<","<<status<<","<<certified<<","<<seconds<<"\n"<<std::flush;
    }
  }
}
