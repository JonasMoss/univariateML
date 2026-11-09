#include "cppunivariate/distributions/st/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main(){
 std::cout<<std::setprecision(17);
 for(int route=0;route<6;++route){
  const std::vector<double> x=route==3?std::vector<double>{-1000,-10,-1,0,1,10,1000}:route==4?std::vector<double>{-2,-1,0,1,2}:(route==2||route==5)?std::vector<double>{2,2,2}:std::vector<double>{-3,-1,-.2,.1,.8,2,4};
  const auto start=std::chrono::steady_clock::now();
  const auto r=cppunivariate::st_mle({x.data(),x.size()},1e-6,200,route==1?0:NAN,route==1||(route>=3&&route<=4)?NAN:1,(route==2||route==5)?NAN:0,route>=3?NAN:4);
  std::cout<<route<<' '<<int(r.status)<<' '<<r.certified<<' '<<r.xi<<' '<<r.omega<<' '<<r.logLik<<' '<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()<<'\n';
 }
}
