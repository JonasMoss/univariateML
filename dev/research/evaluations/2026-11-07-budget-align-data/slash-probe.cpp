#include "cppunivariate/distributions/slash/mle.hpp"
#include <iostream>
#include <chrono>
int main(){std::vector<double>x{0,1};for(int it:{200,500,1000,2000,7813}){auto t=std::chrono::steady_clock::now();auto f=cppunivariate::slash_mle({x.data(),x.size()},1e-9,it);std::cout<<it<<" "<<int(f.status)<<" "<<f.budget_exhausted<<" "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count()<<std::endl;}}
