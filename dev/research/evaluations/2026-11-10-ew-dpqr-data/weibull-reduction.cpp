#include "cppunivariate/distributions/expweibull/dpqr.hpp"
#include "cppunivariate/distributions/weibull/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main(){for(double b:{1.,.01,1e-200,1e-308}) std::cout<<std::setprecision(17)<<b<<" "<<cppunivariate::distributions::qexpweibull(-1,1,b,1,false,true)<<" "<<cppunivariate::distributions::qweibull(-1,b,1,false,true)<<"\n";}
