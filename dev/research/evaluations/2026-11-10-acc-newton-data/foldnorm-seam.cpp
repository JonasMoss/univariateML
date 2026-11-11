#include "cppunivariate/distributions/foldnorm/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){const double e=.0028867513459481287;const std::vector<double>x{e,e,1};
const auto r=cppunivariate::foldnorm_mle({x.data(),x.size()});
std::cout<<std::setprecision(17)<<"{\"epsilon\":"<<e<<",\"mu\":"<<r.mu<<",\"sigma\":"<<r.sigma<<",\"logLik\":"<<r.logLik<<",\"status\":"<<int(r.status)<<"}\n";}
