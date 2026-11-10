#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/invgamma/dpqr.hpp"
#include "cppunivariate/distributions/lgamma/dpqr.hpp"
#include "cppunivariate/distributions/naka/dpqr.hpp"
#include "cppunivariate/distributions/lnorm/dpqr.hpp"
#include "cppunivariate/distributions/logitnorm/dpqr.hpp"
#include "cppunivariate/distributions/llogis/dpqr.hpp"
#include "cppunivariate/distributions/invweibull/dpqr.hpp"
#include "cppunivariate/distributions/invburr/dpqr.hpp"
#include <iostream>
#include <iomanip>
#include <string>
int main(){
  std::cout<<std::setprecision(17);
  std::string family;int index,n;double a,b,s;
  while(std::cin>>family>>index>>n>>a>>b>>s){double sum=0;
    for(int i=0;i<n;++i){double v;std::cin>>v;
      if(family=="invgamma")sum+=cppunivariate::distributions::dinvgamma(v,a,b,true);
      if(family=="lgamma")sum+=cppunivariate::distributions::dlgamma(v,a,b,true);
      if(family=="naka")sum+=cppunivariate::distributions::dnaka(v,a,b,true);
      if(family=="lnorm")sum+=cppunivariate::distributions::dlnorm(v,a,b,true);
      if(family=="logitnorm")sum+=cppunivariate::distributions::dlogitnorm(v,a,b,true);
      if(family=="llogis")sum+=cppunivariate::distributions::dllogis(v,a,b,true);
      if(family=="invweibull")sum+=cppunivariate::distributions::dinvweibull(v,a,b,true);
      if(family=="invburr")sum+=cppunivariate::distributions::dinvburr(v,a,b,s,true);
    }
    std::cout<<family<<","<<index<<","<<sum<<"\n";
  }
}
