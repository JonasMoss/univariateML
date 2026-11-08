#include "cppunivariate/special/skewed_densities.hpp"
#include "cppunivariate/distributions/sstd/mle.hpp"
#include "cppunivariate/distributions/sstd/dpqr.hpp"
#include "cppunivariate/distributions/sged/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main() {
 std::cout<<std::setprecision(17);
 double nu;
 while(std::cin>>nu) {
  double mu=0;
  std::cout<<nu<<' '<<cppunivariate::dens::std_abs_moment(nu)<<' '
   <<cppunivariate::dens::log_dstd_unit_const(nu)<<' '
   <<cppunivariate::dens::ged_abs_moment(nu)<<' '
   <<cppunivariate::dens::ged_lambda(nu)<<' '
   <<cppunivariate::distributions::dsstd(0,0,1,nu,2,false)<<' '
   <<cppunivariate::distributions::dsged(0,0,1,nu,2,false)<<' '
   <<cppunivariate::dens::sstd_sigma(nu,2,mu)<<' '<<mu<<'\n';
 }
 const std::vector<double> x{-2,-1,0,1,3};
 for(double v:{128.,1e6,1e12,1e17}) {
  const auto r=cppunivariate::sstd_mle({x.data(),x.size()},1e-6,200,0,1,v);
  std::cerr<<"fit "<<v<<" status="<<static_cast<int>(r.status)<<" certified="<<r.certified<<" xi="<<r.xi<<" reason="<<r.certificate_reason<<'\n';
 }
}
