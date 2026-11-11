#include <cppunivariate/distributions/gengamma/dpqr.hpp>
#include <cmath>
#include <iomanip>
#include <iostream>
int main() {
  std::cout<<std::setprecision(17);
  for (double shape : {1e4,1e6,1e8,1e10,1e12,1e200}) {
    const double loga=-2*std::log(shape);
    for (double lx : {-50.,-36.1,-35.9,-5.,0.,1.,4.,7.}) {
      const double x=std::exp(lx);
      const double lp=cppunivariate::distributions::pgengamma(x,loga,shape,shape,false,true);
      const double quant=cppunivariate::distributions::qgengamma(lp,loga,shape,shape,false,true);
      std::cout<<shape<<' '<<lx<<' '<<lp<<' '<<quant<<'\n';
    }
  }
}
