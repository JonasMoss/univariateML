#include "cppunivariate/cppunivariate.hpp"
#include "json_loader.hpp"
#include <iostream>
#include <random>
int main(){
 for(int seed=0;seed<40;++seed){
  std::mt19937 rng(143+seed);std::student_t_distribution<double> d(seed%4==0?1:seed%4==1?4:seed%4==2?30:300);
  std::vector<double>x;for(int i=0;i<40;++i)x.push_back(d(rng));
  for(int it:{1,2,3,5,10,20}){
   auto a=cppunivariate::lst_mle({x.data(),x.size()},1e-6,it);
   if(a.status==cppunivariate::LstStatus::CertificateBudget){
    std::cout<<"FOUND "<<seed<<" "<<it<<" "<<a.iterations<<std::endl;
    for(double v:x)std::cout.precision(17),std::cout<<v<<",";std::cout<<std::endl;return 0;
   }
  }
  std::cout<<"seed "<<seed<<std::endl;
 }
}
