#include "cppunivariate/distributions/frechet/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
 for(double lambda:{1e4,1e6,1e8,1e9}){
  std::vector<double>x;for(int i=0;i<80;++i)x.push_back(std::pow(-std::log((i+.5)/80),-1/lambda));
  const double m=x.front();for(double& d:x)d-=m;
  cppunivariate::FrechetResult fit{lambda,-m,1,0,0,cppunivariate::FrechetStatus::Ok};
  fit=cppunivariate::frechet_mle({x.data(),x.size()},1e-10,100,NAN,NAN,1);
  bool ok=fit.status==cppunivariate::FrechetStatus::Ok;
  std::cout<<"{\"sample_id\":"<<int(std::log10(lambda))<<",\"family\":\"frechet\",\"hold\":\"sigma\",\"status\":"<<(ok?0:5)<<",\"certified\":"<<fit.certified<<",\"check_accuracy\":true,\"coef\":["<<fit.lambda<<','<<fit.mu<<",1],\"data\":[";
  for(std::size_t j=0;j<x.size();++j)std::cout<<(j?",":"")<<x[j];std::cout<<"]}\n";
 }
}
