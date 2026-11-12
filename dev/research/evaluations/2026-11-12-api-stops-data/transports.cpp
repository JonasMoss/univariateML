#include "cppunivariate/distributions/pareto2/mle.hpp"
#include "cppunivariate/distributions/invparalogis/mle.hpp"
#include <iostream>
int main(){
 const double x[]={1e308,1.1e308,1.2e308};
 for(double tol:{1e-10,1e-20}){
  const auto p=cppunivariate::pareto2_mle({x,3},0,tol,200,100,NAN);
  const auto i=cppunivariate::invparalogis_mle_holds({x,3},.5,NAN,tol,200);
  std::cout<<"pareto2 tol="<<tol<<" status="<<int(p.status)<<" scale="<<p.scale<<" finite_loglik="<<std::isfinite(p.logLik)<<"\n";
  std::cout<<"invparalogis tol="<<tol<<" status="<<int(i.status)<<" scale="<<i.scale<<" finite_loglik="<<std::isfinite(i.logLik)<<"\n";
 }
}
