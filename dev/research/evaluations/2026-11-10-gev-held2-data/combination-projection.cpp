#include "cppunivariate/distributions/frechet/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){const double unit=200000*std::numeric_limits<double>::denorm_min();
 const std::vector<double>x{-unit,unit,-4*unit};const cppunivariate::span s{x.data(),x.size()};
 std::cout<<std::setprecision(17)<<"route,status,certified,mu_over_unit,sigma_over_unit,shape\n";
 for(bool held:{false,true}){const auto r=cppunivariate::gev_mle(s,1e-10,100,NAN,held?0:NAN,NAN);std::cout<<(held?"gev_mu":"gev_all")<<','<<int(r.status)<<','<<r.certified<<','<<r.mu/unit<<','<<r.sigma/unit<<','<<r.xi<<'\n';}
 const auto f=cppunivariate::frechet_mle(s,1e-10,100,NAN,NAN,unit);std::cout<<"frechet_sigma,"<<int(f.status)<<','<<f.certified<<','<<f.mu/unit<<','<<f.sigma/unit<<','<<f.lambda<<'\n';
}
