#include <cppunivariate/distributions/sn/mle.hpp>
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);const std::vector<double>x{-1.7,-.9,-.2,.15,.3,.8,1.6,3.2};for(double a:{-1000.,-30.,-2.,0.,2.,30.,1000.})for(int m=0;m<3;++m){double xi=m==1?.2:NAN,w=m==2?1.3:NAN;auto f=cppunivariate::sn_detail::inner(x,a,xi,w,1e-10,200);auto c=cppunivariate::sn_detail::acceptance(x,a,xi,w,f.eta,f.h);std::cout<<a<<" "<<m<<" "<<f.eta<<" "<<f.h<<" "<<f.ll<<" "<<f.converged<<" "<<c.gap<<" "<<c.eta_error<<" "<<c.h_error<<"\n";}}
