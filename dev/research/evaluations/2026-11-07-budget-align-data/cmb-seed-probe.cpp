#include "cppunivariate/distributions/cmb/mle.hpp"
#include <iostream>
#include <random>
int main(){std::vector<int>x{0,0,1,1,2,2,2,3,3,4};for(int I:{100,500,1000,5000})for(double nu:{-1.,0.,.5}){auto r=cppunivariate::cmb_size_fixed_nu_mle({x.data(),x.size()},nu,1e-10,I);std::cout<<I<<" "<<nu<<" "<<int(r.fit.status)<<" "<<r.fit.stop_detail.budget_exhausted<<" "<<r.size<<" "<<r.fit.stop_detail.candidates.size()<<std::endl;}std::mt19937 rng(1142026);std::binomial_distribution<int>d(4,.4);x.resize(100);for(int&v:x)v=d(rng);for(int I:{100,500,1000,5000}){auto r=cppunivariate::cmb_size_fixed_nu_mle({x.data(),x.size()},.5,1e-10,I);std::cout<<"seed "<<I<<" "<<int(r.fit.status)<<" "<<r.fit.stop_detail.budget_exhausted<<" "<<r.size<<std::endl;}}
