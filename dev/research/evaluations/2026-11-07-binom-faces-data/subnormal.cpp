#include "cppunivariate/distributions/zib/mle.hpp"
#include "cppunivariate/distributions/zib/dpqr.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
int main() {
 const std::vector<int> x{0,500};
 const double pi=std::numeric_limits<double>::denorm_min();
 const auto f=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,1075,.5,pi);
 double ll=0;for(int v:x)ll+=cppunivariate::distributions::dzib(v,1075,.5,pi,true);
 std::cout<<std::setprecision(17)<<"status="<<int(f.status)<<" fit="<<f.logLik<<" density="<<ll<<" difference="<<f.logLik-ll<<"\n";
 std::vector<int> y(1000,535);y.push_back(0);
 const auto g=cppunivariate::zib_mle({y.data(),y.size()},1e-10,200,1075,NAN,pi);
 double gl=0;for(int v:y)gl+=cppunivariate::distributions::dzib(v,1075,g.prob,pi,true);
 std::cout<<"free probability status="<<int(g.status)<<" prob="<<g.prob<<" fit="<<g.logLik<<" density="<<gl<<" difference="<<g.logLik-gl<<"\n";
 const std::vector<int> zero{0};
 const auto allzero=cppunivariate::zib_mle({zero.data(),zero.size()},1e-10,200,1,1e-300);
 const auto tiny=cppunivariate::zib_mle({zero.data(),zero.size()},1e-10,200,1,1e-300,.5);
 const auto overflow=cppunivariate::zib_mle({zero.data(),zero.size()},1e-10,200,1e308,.9,0);
 std::cout<<"allzero status="<<int(allzero.status)<<" pi="<<allzero.pi<<" ll="<<allzero.logLik<<" tiny_ll="<<tiny.logLik<<" range_status="<<int(overflow.status)<<" range_upper="<<overflow.stop_detail.loglik_upper<<"\n";
}
