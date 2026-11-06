// stdin: one n followed by n exact decimal binary64 observations per sample.
#include "cppunivariate/distributions/lst/certificate.hpp"
#include "cppunivariate/distributions/std/mle.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
int main(){
  std::cout<<std::setprecision(17);std::size_t n;
  while(std::cin>>n){std::vector<double>x(n);for(double&v:x)std::cin>>v;
    for(double q:{1.,2.}){
      auto start=std::chrono::steady_clock::now();
      cppunivariate::TCertificateResult r;double seconds=0;
      for(int j=0;j<4;++j){start=std::chrono::steady_clock::now();r=cppunivariate::t_certificate({x.data(),x.size()},q);if(j)seconds+=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()/3;}
      double oldseconds=0;int oldstatus=0;double oldmu=0,oldscale=0,oldnu=0,oldll=0;
      for(int j=0;j<4;++j){start=std::chrono::steady_clock::now();
        if(q==1){auto old=cppunivariate::lst_mle_impl({x.data(),x.size()},1.220703e-4,200);oldstatus=int(old.status);oldmu=old.mu;oldscale=old.sigma;oldnu=old.df;oldll=old.logLik;}
        else{auto old=cppunivariate::student_t_mle_impl({x.data(),x.size()},1.220703e-4,200,false);oldstatus=int(old.status);oldmu=old.mu;oldscale=old.sigma;oldnu=old.nu;oldll=old.logLik;}
        if(j)oldseconds+=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()/3;
      }
      std::cout<<int(q)<<" "<<int(r.status)<<" "<<r.mu<<" "<<r.scale<<" "<<r.nu<<" "<<r.logLik<<" "<<r.profiles<<" "<<seconds;
      for(int j=0;j<3;++j)std::cout<<" "<<r.coefficient_lower[j]<<" "<<r.coefficient_upper[j];
      std::cout<<" "<<oldstatus<<" "<<oldseconds<<" "<<oldmu<<" "<<oldscale<<" "<<oldnu<<" "<<oldll<<std::endl;
    }
  }
}
