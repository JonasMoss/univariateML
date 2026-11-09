// stdin rows: n mean sd p q iterlim x[1] ... x[n]; free holds use "nan".
#include "cppunivariate/distributions/ged/mle.hpp"
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>
int main() {
  std::size_t n;std::string mean,sd;double p,q;int maxit;
  std::cout<<std::setprecision(17);
  while(std::cin>>n>>mean>>sd>>p>>q>>maxit) {
    std::vector<double>x(n);for(double& v:x)std::cin>>v;
    const cppunivariate::span data{x.data(),x.size()};
    const double m=std::strtod(mean.c_str(),nullptr),s=std::strtod(sd.c_str(),nullptr);
    const auto location=cppunivariate::ged_shape_detail::locations(data,p,q,m,maxit);
    const auto moment=cppunivariate::ged_shape_detail::moment(data,p,m,maxit,location.lo,location.hi);
    const auto interval=cppunivariate::ged_shape_detail::interval(data,p,q,m,s,maxit,location.lo,location.hi);
    const auto tail=cppunivariate::ged_shape_detail::tail(data,p,m,s,maxit);
    std::cout<<moment.log_lower<<" "<<moment.log_upper<<" "<<interval.upper<<" "<<tail.upper
             <<" "<<moment.valid<<" "<<interval.valid<<" "<<tail.valid<<" "<<tail.endpoint_uncertain<<"\n";
  }
}
