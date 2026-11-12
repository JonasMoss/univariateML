#include "cppunivariate/distributions/cmb/mle.hpp"
#include "cppunivariate/distributions/dgamma/mle.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
int main(){std::string id,s;int n;double size,prob,nu,shape,rate;std::cin>>id>>n;std::vector<double>x(n);for(double&v:x)std::cin>>v;
const auto read=[](){std::string s;std::cin>>s;return std::strtod(s.c_str(),nullptr);};size=read();prob=read();nu=read();shape=read();rate=read();
std::cout<<std::setprecision(17);const auto start=std::chrono::steady_clock::now();
std::vector<int> ix(x.begin(),x.end());cppunivariate::ispan xs{ix.data(),ix.size()};
if(id=="cmb"){cppunivariate::CmbSizeResult r;if(!std::isnan(size))r={size,cppunivariate::cmb_mle(xs,int(size),1e-10,2000,prob,nu)};else if(std::isnan(nu))r=cppunivariate::cmb_size_free_nu_mle(xs,prob);else if(std::isnan(prob))r=cppunivariate::cmb_size_fixed_nu_mle(xs,nu);else r=cppunivariate::cmb_size_mle(xs,prob,nu);
std::cout<<int(r.fit.status)<<' '<<r.size<<' '<<r.fit.prob<<' '<<r.fit.nu<<' '<<r.fit.logLik<<' '<<r.fit.iterations<<' '<<r.fit.stop_detail.budget_exhausted<<' '<<r.fit.stop_detail.candidates.size();}
else{const auto r=cppunivariate::dgamma_mle({x.data(),x.size()},1e-10,400,shape,rate);std::cout<<int(r.status)<<' '<<r.shape<<' '<<r.rate<<' '<<r.logLik<<' '<<r.iterations<<' '<<r.stop_detail.budget_exhausted<<' '<<r.stop_detail.candidates.size();}
std::cout<<' '<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<'\n';}
