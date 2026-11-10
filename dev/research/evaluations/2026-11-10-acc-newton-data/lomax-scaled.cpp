#include "cppunivariate/distributions/lomax/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);int i=0;
for(auto x:std::vector<std::vector<double>>{{1,100},{.001,4,6},{.001,1,2},{1,29},{1,1,1,12,12}}){
 for(double scale:{1.,1e-200,1e200,1e307}){
  auto y=x;bool valid=true;for(double& v:y){v*=scale;valid=valid&&std::isfinite(v);}if(!valid)continue;
  auto r=cppunivariate::lomax_mle({y.data(),y.size()},1e-12,200);
  std::cout<<i<<' '<<scale<<' '<<int(r.status)<<' '<<r.lambda*scale<<' '<<r.kappa<<' '<<r.iterations<<' '<<int(r.certificate_failure)<<'\n';
 }++i;
}}
