#include "cppunivariate/distributions/invparalogis/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){
 std::cout<<std::setprecision(17);
 int fail=0;
 for(double d:{.1,.01,.003,.001,.0001,.00001}){
  std::vector<double> x{std::exp(-d)};
  auto r=cppunivariate::paralogis_mle_fixed_scale({x.data(),x.size()},1,1e-10,1000);
  auto c=cppunivariate::paralogis_cert_detail::certify({x.data(),x.size()},r.shape,1,true);
  std::cout<<d<<'\t'<<int(r.status)<<'\t'<<r.shape<<'\t'<<r.logLik<<'\t'<<c.ok<<'\t'<<c.boxes<<'\n';
  if(r.status!=cppunivariate::ParalogisStatus::Ok)++fail;
 }
 return fail?1:0;
}
