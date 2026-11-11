#include "cppunivariate/distributions/gb2/mle.hpp"
#include "cppunivariate/distributions/sged/mle.hpp"
#include <iostream>
int main(){
 const std::vector<double> x{std::exp(-1.1),std::exp(-1.)};cppunivariate::span d{x.data(),x.size()};
 const auto b=cppunivariate::burr_mle_fixed_scale(d,1);
 const auto g=cppunivariate::gb2_mle(d,1e-8,1000,NAN,NAN,1,1);
 std::cout<<"burr\t"<<int(b.status)<<'\t'<<b.certified<<"\ngb2-unit-shape\t"<<int(g.status)<<'\t'<<g.certified<<'\n';
 const std::vector<double> y{.7321666961121761,2.2766042947262384,-.5491691631533211};cppunivariate::span e{y.data(),y.size()};
 const auto p=cppunivariate::ged_mle(e,1e-8,1000,NAN,1.0178559902092952,NAN);
 const auto s=cppunivariate::sged_mle(e,1e-8,1000,NAN,1.0178559902092952,NAN,1);
 std::cout<<"ged\t"<<int(p.status)<<'\t'<<p.certified<<"\nsged-held-xi\t"<<int(s.status)<<'\t'<<s.certified<<'\n';
}
