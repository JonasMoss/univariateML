#include "cppunivariate/distributions/snorm/mle.hpp"
#include <chrono>
#include <iostream>
#include <random>
int main(){std::mt19937 gen(11624);std::normal_distribution<double> nd;std::vector<double>x;for(int i=0;i<100;++i)x.push_back(nd(gen));for(int mask=0;mask<8;++mask){auto start=std::chrono::steady_clock::now();cppunivariate::SnormResult r{};for(int j=0;j<3;++j)r=cppunivariate::snorm_mle({x.data(),x.size()},1e-9,200,mask&1?.2:NAN,mask&2?1.4:NAN,mask&4?1.3:NAN);std::cout<<mask<<" "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()/3<<" "<<int(r.status)<<" "<<r.mu<<" "<<r.sigma<<" "<<r.xi<<" "<<r.logLik<<"\n";}}
