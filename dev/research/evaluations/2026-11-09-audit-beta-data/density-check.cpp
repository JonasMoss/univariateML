// Diagnostic for the protected density kernels at the repaired fit points.
#include "cppunivariate/distributions/betapr/dpqr.hpp"
#include "cppunivariate/distributions/nsbeta/dpqr.hpp"
#include "cppunivariate/distributions/pareto4/dpqr.hpp"
#include <iostream>
#include <iomanip>
#include <string>
int main() {
  int index=0,n=0;
  std::string family;
  double a=0,b=0,s=0,lo=0,hi=0;
  std::cout<<std::setprecision(17);
  while(std::cin>>index>>family>>n>>a>>b>>s>>lo>>hi) {
    double sum=0;
    for(int i=0;i<n;++i) {
      double x=0;std::cin>>x;
      if(family=="betapr")sum+=cppunivariate::distributions::dbetapr(x,a,b,s,true);
      else if(family=="nsbeta")sum+=cppunivariate::distributions::dnsbeta(x,a,b,lo,hi,true);
      else sum+=cppunivariate::distributions::dpareto4(x,a,b,s,lo,true);
    }
    std::cout<<index<<','<<family<<','<<sum<<'\n';
  }
}
