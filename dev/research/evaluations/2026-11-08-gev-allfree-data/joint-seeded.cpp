#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <random>
#include <iostream>
#include <iomanip>
int main(){std::mt19937_64 engine(20261029);int count=0,failures=0,maxwork=0;std::cout<<std::setprecision(17);for(double shape:{-.9,-.6,-.3,0.,.1,.3,.7,1.,2.})for(int n:{3,4,5,10,30,100,500})for(int replicate=0;replicate<3;++replicate){std::vector<double>x;for(int i=0;i<n;++i){double p=(static_cast<double>(engine()>>11)+.5)/9007199254740992.;x.push_back(cppunivariate::distributions::qgev(p,0,1,shape,true,false));}std::sort(x.begin(),x.end());double m=x.front(),d=x.back()-m;for(double&v:x)v=(v-m)/d;auto f=cppunivariate::gev_mle({x.data(),x.size()});++count;maxwork=std::max(maxwork,f.iterations);std::cout<<shape<<" "<<n<<" "<<replicate<<" status "<<int(f.status)<<" work "<<f.iterations<<" budget "<<f.stop_detail.budget_exhausted<<std::endl;if(f.status!=cppunivariate::GevStatus::Ok){++failures;for(double v:x)std::cout<<v<<",";std::cout<<std::endl;}}std::cout<<"cases "<<count<<" failures "<<failures<<" maximum_evaluations "<<maxwork<<std::endl;return failures?1:0;}
