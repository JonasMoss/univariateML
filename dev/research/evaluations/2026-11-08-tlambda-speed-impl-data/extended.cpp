#ifdef BASELINE
#include "baseline.hpp"
#else
#include "cppunivariate/distributions/tlambda/mle.hpp"
#endif
#include <iostream>
#include <iomanip>
int main(int argc,char**){std::cout<<std::setprecision(17);
for(double shape:{-2.,-8.,-20.}){
 std::vector<double>x;for(int i=0;i<30;++i)x.push_back(cppunivariate::distributions::qtlambda((i+.5)/30,0,1,shape));
 if(argc>1){std::cout<<shape<<' '<<x.size();for(double v:x)std::cout<<' '<<v;std::cout<<'\n';continue;}
 for(int mask:{1,4}){
 auto f=cppunivariate::tlambda_mle({x.data(),x.size()},1e-10,2000,mask==4?shape:NAN,mask==1?0:NAN,NAN);
 std::cout<<shape<<' '<<mask<<' '<<int(f.status)<<' '<<f.mu<<' '<<f.sigma<<' '<<f.lambda<<' '<<f.logLik<<'\n';
 }
}}
