// Regression evidence for padding before endpoint-order validation.
#include "cppunivariate/distributions/paralogis/certificate.hpp"
#include <iostream>
#include <iomanip>
int main(){
  const double y=-.8+8022./10000;
  const auto z=cppunivariate::paralogis_cert_detail::product(.5,y);
  const double lo=cppunivariate::burr_cert_detail::softplus(z.lo);
  const double hi=cppunivariate::burr_cert_detail::softplus(z.hi);
  const auto enclosed=cppunivariate::paralogis_cert_detail::relative(lo,hi);
  std::cout<<std::setprecision(17)<<"z_lo\tz_hi\trounded_softplus_lo\trounded_softplus_hi\tenclosure_lo\tenclosure_hi\n";
  std::cout<<z.lo<<'\t'<<z.hi<<'\t'<<lo<<'\t'<<hi<<'\t'<<enclosed.lo<<'\t'<<enclosed.hi<<'\n';
  return enclosed.valid()&&enclosed.lo<=std::min(lo,hi)&&enclosed.hi>=std::max(lo,hi)?0:1;
}
