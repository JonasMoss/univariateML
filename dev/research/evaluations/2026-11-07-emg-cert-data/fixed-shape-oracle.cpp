#include "cppunivariate/distributions/emg/fixed_shape_certificate.hpp"
#include <iostream>
#include <iomanip>
int main() {
  const std::vector<double> x{-2,-1,0,1,2};
  std::cout<<std::setprecision(17)<<"mask,k,mu,b,lower,upper,b_lower,b_upper,u_lower,u_upper,ok\n";
  for(double k:{.1,1.5,10.})for(int mask=0;mask<8;++mask) {
    const auto c=cppunivariate::emg_detail::fixed_shape_certificate({x.data(),x.size()},k,1e-7,500,
      mask&1?0:NAN,mask&2?1:NAN,mask&4?k:NAN);
    std::cout<<mask<<','<<k<<','<<c.mu<<','<<c.b<<','<<c.loglik_lower<<','<<c.loglik_upper<<','
      <<c.b_lower<<','<<c.b_upper<<','<<c.u_lower<<','<<c.u_upper<<','<<c.ok<<'\n';
  }
}
