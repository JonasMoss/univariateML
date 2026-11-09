#include "json_loader.hpp"
#include "cppunivariate/distributions/cmp/mle.hpp"
#include "cppunivariate/distributions/lomax/mle.hpp"
#include <iostream>
#include <iomanip>
int main() {
 std::cout<<std::setprecision(17);
 const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/cmp/golden.json");
 for(bool fl:{false,true})for(bool fn:{false,true}) {
  cppunivariate::CmpFitControl ctl;
  if(fl)ctl.fixed_lambda=g.estimates.at("lambda");
  if(fn)ctl.fixed_nu=g.estimates.at("nu");
  auto f=cppunivariate::cmp_domain_mle({g.data.data(),g.data.size()},ctl);
  std::cout<<"{\"id\":\"cmp-held-"<<fl<<fn<<"\",\"lambda_error\":"<<std::abs(std::exp(f.eta)/g.estimates.at("lambda")-1)
    <<",\"nu_error\":"<<std::abs(f.nu/g.estimates.at("nu")-1)<<",\"status\":"<<int(f.status)<<"}\n";
 }
 const auto l=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/lomax/golden.json");
 std::vector<double> x;for(int i=0;i<int(std::ceil(4097./l.data.size()));++i)x.insert(x.end(),l.data.begin(),l.data.end());
 const auto f=cppunivariate::lomax_mle({x.data(),x.size()},1e-10,100);
 std::cout<<"{\"id\":\"lomax-compressed\",\"lambda_error\":"<<std::abs(f.lambda/l.estimates.at("lambda")-1)
   <<",\"kappa_error\":"<<std::abs(f.kappa/l.estimates.at("kappa")-1)<<",\"status\":"<<int(f.status)<<"}\n";
}
