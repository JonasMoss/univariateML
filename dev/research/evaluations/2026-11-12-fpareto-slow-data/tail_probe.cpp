#include "cppunivariate/distributions/burr/dpqr.hpp"
#include "cppunivariate/distributions/invburr/dpqr.hpp"
#include <iostream>
#include <iomanip>
#include <cstdlib>
int main(int argc,char**argv) {
 const double q=argc>1?std::strtod(argv[1],nullptr):1e100;
 const double a=argc>2?std::strtod(argv[2],nullptr):1e-308;
 const double b=argc>3?std::strtod(argv[3],nullptr):1e308;
 const double ratio=cppunivariate::special::log_positive_ratio(q,1);
 std::cout<<std::setprecision(17)<<"ratio "<<ratio<<" product "<<b*ratio<<" logproduct "<<cppunivariate::special::log_softplus_product(b,ratio)<<'\n';
 const double lh=std::log(a)+cppunivariate::special::log_softplus_product(b,ratio);
 const auto tails=cppunivariate::special::tails_from_log_hazard(lh,false);
 std::cout<<"loga "<<std::log(a)<<" lh "<<lh<<" tails "<<tails.lower<<' '<<tails.upper<<'\n';
 std::cout<<"runtime "<<cppunivariate::distributions::pburr(q,a,b,1,false,true)<<' '<<cppunivariate::distributions::pinvburr(1/q,a,b,1,true,true)<<'\n';
 std::cout<<"constant "<<cppunivariate::distributions::pburr(1e100,1e-308,1e308,1,false,true)<<' '<<cppunivariate::distributions::pinvburr(1e-100,1e-308,1e308,1,true,true)<<'\n';
}
