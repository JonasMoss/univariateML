#include "cppunivariate/distributions/sged/mle.hpp"
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include "json_loader.hpp"
int main(){
 const auto x=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/sged/golden.json").data;
 std::cout<<std::setprecision(17)<<"scope\tmask\tstatus\tcertified\tmean\tsd\tnu\txi\tll\tms\twork\n";
 for(int scope=0;scope<3;++scope)for(int mask=0;mask<4;++mask){
 const double mu=mask&1?2:NAN,sd=mask&2?3:NAN,p=scope==1?4:NAN,xi=scope==0?1.4:NAN;
 const auto start=std::chrono::steady_clock::now();
 const auto r=cppunivariate::sged_mle({x.data(),x.size()},1e-10,200,mu,sd,p,xi);
 std::cout<<scope<<'\t'<<mask<<'\t'<<int(r.status)<<'\t'<<r.certified<<'\t'<<r.mu<<'\t'<<r.sigma<<'\t'<<r.nu<<'\t'<<r.xi<<'\t'<<r.logLik<<'\t'<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()<<'\t'<<r.iterations<<'\n';
 }
}
