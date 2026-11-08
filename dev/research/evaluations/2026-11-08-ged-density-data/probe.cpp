#include <cppunivariate/distributions/sged/dpqr.hpp>
#include <iomanip>
#include <iostream>
int main() {
  std::cout << std::setprecision(17);
  for (double nu : {.02,.0005}) for (double x : {-1e-100,0.,1e-100}) {
    std::cout << "ged " << x << " 0 1e300 " << nu << " 1 "
      << cppunivariate::distributions::dged(x,0,1e300,nu,true) << ' '
      << cppunivariate::distributions::pged(x,0,1e300,nu,true,true) << ' '
      << cppunivariate::distributions::pged(x,0,1e300,nu,false,true) << '\n';
  }
  for (double xi : {1.,1.3}) for (double x : {-1e-100,1e-100}) {
    std::cout << "sged " << x << " 0 1e300 .02 " << xi << ' '
      << cppunivariate::distributions::dsged(x,0,1e300,.02,xi,true) << ' '
      << cppunivariate::distributions::psged(x,0,1e300,.02,xi,true,true) << ' '
      << cppunivariate::distributions::psged(x,0,1e300,.02,xi,false,true) << '\n';
  }
  for (double x : {-1e-100,1e-100}) {
    std::cout << "sged " << x << " 0 1e300 .0005 1 "
      << cppunivariate::distributions::dsged(x,0,1e300,.0005,1,true) << ' '
      << cppunivariate::distributions::psged(x,0,1e300,.0005,1,true,true) << ' '
      << cppunivariate::distributions::psged(x,0,1e300,.0005,1,false,true) << '\n';
  }
  for (double xi : {1.,1.3}) {
    std::cout << "sged 1e-23 0 1e300 .02 " << xi << ' '
      << cppunivariate::distributions::dsged(1e-23,0,1e300,.02,xi,true) << ' '
      << cppunivariate::distributions::psged(1e-23,0,1e300,.02,xi,true,true) << ' '
      << cppunivariate::distributions::psged(1e-23,0,1e300,.02,xi,false,true) << '\n';
  }
  std::cout << "ged 1e-23 0 1e300 .02 1 "
      << cppunivariate::distributions::dged(1e-23,0,1e300,.02,true) << ' '
      << cppunivariate::distributions::pged(1e-23,0,1e300,.02,true,true) << ' '
      << cppunivariate::distributions::pged(1e-23,0,1e300,.02,false,true) << '\n';
  for (double x : {-1e308,1e308}) {
    const double mean=-x;
    std::cout << "sged " << x << ' ' << mean << " 1e308 .02 1.3 "
      << cppunivariate::distributions::dsged(x,mean,1e308,.02,1.3,true) << ' '
      << cppunivariate::distributions::psged(x,mean,1e308,.02,1.3,true,true) << ' '
      << cppunivariate::distributions::psged(x,mean,1e308,.02,1.3,false,true) << '\n';
  }
  for (double nu : {.0003,.00036}) for (double xi : {.7,1.3}) for (double x : {0.,1e-100}) {
    std::cout << "sged " << x << " 0 1e300 " << nu << ' ' << xi << ' '
      << cppunivariate::distributions::dsged(x,0,1e300,nu,xi,true) << ' '
      << cppunivariate::distributions::psged(x,0,1e300,nu,xi,true,true) << ' '
      << cppunivariate::distributions::psged(x,0,1e300,nu,xi,false,true) << '\n';
  }
  for (double nu : {.0003,.00036}) for (double xi : {.7,1.3}) {
    std::cout << "quantile " << xi << ' ' << nu << ' '
      << cppunivariate::distributions::qsged(.1,0,1e300,nu,xi,true,false) << '\n';
  }

}
