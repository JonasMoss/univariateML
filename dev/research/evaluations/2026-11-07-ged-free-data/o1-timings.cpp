#ifdef GED_O1_BASELINE
#include "o1-baseline-mle.hpp"
#else
#include "cppunivariate/distributions/ged/mle.hpp"
#endif
#include "cppunivariate/distributions/ged/dpqr.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main(){std::vector<double>golden;double v;while(std::cin>>v)golden.push_back(v);std::cout<<std::setprecision(17);for(int sample=0;sample<2;++sample){std::vector<double>x=golden;if(sample){x.clear();for(int j=0;j<10000;++j)x.push_back(cppunivariate::distributions::qged((j+.5)/10000.,0,1,3,true,false));}for(int mask=0;mask<4;++mask){for(int rep=0;rep<(sample?3:5);++rep){auto t=std::chrono::steady_clock::now();const auto r=cppunivariate::ged_mle({x.data(),x.size()},1.220703e-4,200,mask&1?(sample?0:-.017698887677594307):NAN,mask&2?(sample?1:1.0178559902092952):NAN,NAN);std::cout<<sample<<" "<<x.size()<<" "<<mask<<" "<<rep<<" "<<int(r.status)<<" "<<r.certified<<" "<<r.mu<<" "<<r.sigma<<" "<<r.nu<<" "<<r.logLik<<" "<<r.iterations<<" "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-t).count()<<std::endl;}}}}
