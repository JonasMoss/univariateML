#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <string>
int main(int argc,char** argv) {
  const std::string version=argc>1?argv[1]:"after";
  std::cout<<"version,family,unit,status,certified,milliseconds,mu,sigma,loglik\n"<<std::setprecision(17);
  for(double unit:{1e-50,1.,1e50}) {
    std::vector<double> x(10,unit);x[0]=-9*unit;
    auto start=std::chrono::steady_clock::now();
    const auto g=cppunivariate::gev_mle({x.data(),x.size()});
    auto ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
    std::cout<<version<<",gev,"<<unit<<','<<int(g.status)<<','<<g.certified<<','<<ms<<','<<g.mu<<','<<g.sigma<<','<<g.logLik<<'\n';
    const double p=.45,m=p*.1/(std::sqrt(p*(1-p))+p);
    x.assign(20,(m+1)*unit);std::fill_n(x.begin(),9,m*unit);
    start=std::chrono::steady_clock::now();
    const auto f=cppunivariate::frechet_mle({x.data(),x.size()});
    ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
    std::cout<<version<<",frechet,"<<unit<<','<<int(f.status)<<','<<f.certified<<','<<ms<<','<<f.mu<<','<<f.sigma<<','<<f.logLik<<'\n';
  }
}
