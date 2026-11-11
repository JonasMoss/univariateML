#include "cppunivariate/distributions/lst/shape_certificate.hpp"
#include "cppunivariate/distributions/norm/mle.hpp"
#include <iostream>
int main(){std::vector<double>x{-1,2};cppunivariate::span v{x.data(),x.size()};cppunivariate::StopDetail d;cppunivariate::t_shape_detail::retain_point(v,.2,1.5297058413452655,8886110.5205078721,false,d);auto n=cppunivariate::norm_mle(v,.2,NAN);auto a=cppunivariate::t_shape_detail::normal_point(v,n.mean,n.sd);std::cout.precision(17);std::cout<<d.loglik_lower<<" "<<d.candidates.front().loglik_upper<<" "<<a.loglik_lower<<" "<<a.candidates.front().loglik_upper<<" "<<cppunivariate::t_shape_detail::dominated(d,a)<<"\n";}
