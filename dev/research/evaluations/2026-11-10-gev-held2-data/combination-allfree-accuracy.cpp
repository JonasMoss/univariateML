// Exercise the same three-coordinate local rule used by all-free fallback,
// independently of whether this sample's outer cover happens to complete.
#include "cppunivariate/distributions/gev/mle.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);
  for(double shape:{-.4,0.,.3,.8}){
    std::vector<double>x;for(int i=0;i<80;++i)x.push_back(cppunivariate::distributions::qgev((i+.5)/80,0,1,shape,true,false));
    const cppunivariate::span data{x.data(),x.size()};auto fit=cppunivariate::gev_mle(data);
    if(fit.status!=cppunivariate::GevStatus::Ok)return 1;
    fit.mu+=1e-7;fit.sigma*=1-1e-7;fit.xi+=1e-7;fit.certified=false;
    if(!cppunivariate::gev_accuracy_detail::polish(data,fit,false,false,1e-10,100))return 2;
    std::cout<<"{\"sample_id\":"<<int(10*(shape+1))<<",\"family\":\"gev\",\"hold\":\"none\",\"status\":0,\"certified\":0,\"coef\":["<<fit.mu<<','<<fit.sigma<<','<<fit.xi<<"],\"data\":[";
    for(std::size_t j=0;j<x.size();++j)std::cout<<(j?",":"")<<x[j];std::cout<<"]}\n";
  }
}
