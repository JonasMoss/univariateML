#include "cppunivariate/distributions/ged/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main() {std::cout<<std::setprecision(17)<<cppunivariate::distributions::dged(1e-100,0,1e300,.02,true)<<'\n';}
