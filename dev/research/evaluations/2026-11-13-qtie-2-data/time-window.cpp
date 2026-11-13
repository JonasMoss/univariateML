#include "cppunivariate/distributions/binom/tbinom_dpqr.hpp"
#include <chrono>
#include <iostream>
int main(){const auto t=std::chrono::steady_clock::now();const double q=cppunivariate::distributions::qtbinom(.8,1e-8,1,INFINITY,1e8);std::cout<<q<<" "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count()<<" seconds\n";}
