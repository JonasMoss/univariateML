#include "cppunivariate/distributions/ged/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){const double a=1e-320;std::vector<double>x{0,0,a};const auto r=cppunivariate::ged_mle({x.data(),x.size()},1e-6,200,NAN,NAN,.5);const double ideal=std::log(a)+.5*std::log(120.)-std::log(36.);std::cout<<std::setprecision(17)<<"status "<<int(r.status)<<" certified "<<r.certified<<" scale "<<r.sigma<<" log_scale_error "<<std::abs(std::log(r.sigma)-ideal)<<"\n";}
