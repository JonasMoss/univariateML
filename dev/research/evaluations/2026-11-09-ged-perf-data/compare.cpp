#include "cppunivariate/distributions/ged/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){
 std::vector<std::vector<double>> samples{{-1,1},{-1,0,1},{0,0,0},{-.8,0,0,0,.9},{-1,-.3,0,.2,.9},{-1,1,1,1}};
 std::vector<double> tie(8,0);for(int j=0;j<23;++j){tie.push_back(-.32998660809120006);tie.push_back(.32998660809120006);}for(int j=0;j<12;++j){tie.push_back(-1.4239025460520294);tie.push_back(1.4239025460520294);}samples.push_back(tie);
 for(int j=0;j<3;++j){std::vector<double>x;for(int i=0;i<31;++i)x.push_back(std::sin((i+.1)*(j+1))+std::cos((i+.3)*.71));samples.push_back(x);}
 std::cout<<std::setprecision(17);
 for(std::size_t sample=0;sample<samples.size();++sample)for(double scale:{.1,1.,10.})for(int mask=0;mask<4;++mask)for(int cap:{1,2,10,200}){
  const auto& x=samples[sample];const auto r=cppunivariate::ged_mle({x.data(),x.size()},1e-10,cap,mask&1?0:NAN,mask&2?scale:NAN,NAN);
  const auto& d=r.stop_detail;
  std::cout<<sample<<" "<<scale<<" "<<mask<<" "<<cap<<" "<<int(r.status)<<" "<<r.certified<<" "<<d.budget_exhausted<<" "<<d.comparison_tie<<" "<<int(d.limit_model)<<" "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" "<<r.logLik<<"\n";
 }
}
