#include "cppunivariate/distributions/dgamma/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){double a,r;std::cin>>a>>r;const std::vector<double>x{1000000,1000001,1000003,1000008,1000011};cppunivariate::dnorm_detail::Cells p;p.init({x.data(),x.size()});double derivative=0;const double g=cppunivariate::dgamma_detail::rate_score(p,a,std::log(r),derivative);std::cout<<std::setprecision(17)<<"{\"score\":"<<g<<",\"derivative\":"<<derivative<<"}\n";}
