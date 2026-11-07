#include "cppunivariate/distributions/cmb/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){const std::vector<int>x{3,1,3,2,0,2,4,1,2,0,2,2,0,1,2,2,3,0,0,2,0,2,0,3,3,2,2,0,1,1,0,1,3,2,1,1,1,2,1,3,1,0,1,0,0,2,2,0,0,2,4,1,3,1,1,1,1,2,2,0,3,2,1,4,0,0,2,1,0,2,1,3,0,2,0,2,1,1,0,2,2,2,0,0,3,1,1,2,0,1,0,3,0,3,2,1,3,2,1,3};
const auto r=cppunivariate::cmb_size_fixed_nu_mle({x.data(),x.size()},.5);
std::cout<<std::setprecision(17)<<"R seed1142026: status="<<int(r.fit.status)<<" budget="<<r.fit.stop_detail.budget_exhausted<<" size="<<r.size<<" LL="<<r.fit.logLik<<std::endl;
if(r.fit.status!=cppunivariate::CmbStatus::Ok || r.size!=4 || r.fit.stop_detail.budget_exhausted)return 1;
double best=-INFINITY;int arg=0;for(int N=4;N<=300;++N){const auto f=cppunivariate::cmb_mle({x.data(),x.size()},N,1e-10,100,NAN,.5);if(f.status!=cppunivariate::CmbStatus::Ok)return 2;if(f.logLik>best){best=f.logLik;arg=N;}}
return arg==4 && std::abs(best-r.fit.logLik)<1e-8 ? 0:3;}
