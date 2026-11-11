#include "cppunivariate/distributions/lst/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
 for(double center:{0.,1e8,1e12})for(double unit:{1.,.001,1e-6})for(double df:{1.,2.,8.,30.}){
  std::vector<double> x;for(int j=0;j<17;++j)x.push_back(center+unit*(std::sin(1.4*j)+.03*j));
  const auto r=cppunivariate::lst_mle({x.data(),x.size()},1e-10,200,NAN,NAN,df);
  if(!r.certified&&r.status==cppunivariate::LstStatus::Ok){
   std::cout<<"{\"center\":"<<center<<",\"unit\":"<<unit<<",\"df\":"<<df<<",\"mu\":"<<r.mu<<",\"sigma\":"<<r.sigma<<",\"data\":[";
   bool first=true;for(double v:x){if(!first)std::cout<<",";first=false;std::cout<<v;}std::cout<<"]}"<<std::endl;
  }
 }
 const std::vector<double> x{-5,-4,-3,-2,-1,1,2,3,4,5};auto r=cppunivariate::lst_mle({x.data(),x.size()},1e-10,200,NAN,NAN,.5);
 std::cerr<<"symmetric "<<int(r.status)<<" "<<r.mu<<" "<<r.sigma<<std::endl;
}
