#include "cppunivariate/distributions/sged/mle.hpp"
#include <iostream>
#include "json_loader.hpp"
#include <iomanip>
int main(){std::cout<<std::setprecision(17);const auto x=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sged/golden.json").data;for(double p:{.5,1.,1.7,4.,10.}){auto r=cppunivariate::sged_mle({x.data(),x.size()},1e-10,200,0,1,p);std::cout<<p<<" "<<int(r.status)<<" "<<r.certified<<" "<<r.xi<<" "<<r.logLik<<" "<<r.iterations<<" "<<r.stop_detail.comparison_tie<<" "<<r.stop_detail.budget_exhausted<<std::endl;}}
