#include "cppunivariate/distributions/triang/mle.hpp"
#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <string>
int main() {
  std::cout << std::setprecision(17);
  int id,n; std::string text;
  while(std::cin>>id>>n) {
    double h[3];
    for(double& v:h) { std::cin>>text;v=std::stod(text); }
    std::vector<double> x(n);for(double& v:x)std::cin>>v;
    const auto r=cppunivariate::triang_mle({x.data(),x.size()},
        std::getenv("CONTRACT")?1e-10:1e-12,100,h[0],h[1],h[2],
        !std::getenv("EXHAUSTIVE"));
    std::cout<<id<<",triang,"<<int(r.status)<<","<<r.logLik<<","<<r.a<<","<<r.b<<","<<r.c<<","<<r.iterations<<","<<r.not_unique<<"\n";
  }
}
