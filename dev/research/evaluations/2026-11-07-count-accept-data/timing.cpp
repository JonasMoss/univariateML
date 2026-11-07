#include <cppunivariate/distributions/tnorm/mle.hpp>
#include <cppunivariate/distributions/cmb/mle.hpp>
#include <cppunivariate/distributions/cmp/mle.hpp>
#include <chrono>
#include <iostream>
int main(){std::vector<double>x{0,0,1,1,2,2,3,4};std::vector<int>y{0,0,1,1,2,2,3,4};for(int family=0;family<3;++family){auto start=std::chrono::steady_clock::now();double ll=0;for(int j=0;j<100;++j){if(family==0)ll=cppunivariate::tnorm_mle({x.data(),x.size()},-2,8).logLik;if(family==1)ll=cppunivariate::cmb_mle({y.data(),y.size()},8).logLik;if(family==2)ll=cppunivariate::cmp_domain_mle({x.data(),x.size()}).logLik;}std::cout<<family<<" "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()/100<<" "<<ll<<"\n";}}
