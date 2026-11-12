#include "cppunivariate/distributions/expweibull/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){const std::vector<double>x{.5,1,2,4};const double b=std::numeric_limits<double>::denorm_min();const auto w=cppunivariate::weibull_mle({x.data(),x.size()},1e-10,100,NAN,b,NAN);const auto e=cppunivariate::expweibull_mle({x.data(),x.size()},1e-10,100,NAN,b,1);std::cout<<std::setprecision(17)<<int(w.status)<<" "<<w.scale<<" EW "<<int(e.status)<<" "<<e.scale<<" expected near "<<std::sqrt(2.)<<"\n";}
