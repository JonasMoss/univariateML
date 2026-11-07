#include "cppunivariate/distributions/zmbinom/mle.hpp"
#include "cppunivariate/distributions/zib/mle.hpp"
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <chrono>
#include <iostream>
int main(){std::vector<int>x{1,2,2,3,3,3,4,4,5,5},z=x;z.insert(z.end(),5,0);const auto run=[&](const char* name,auto f){const auto start=std::chrono::steady_clock::now();for(int j=0;j<10;++j)f();std::cout<<name<<" "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()/10<<"\n";};run("ztbinom",[&]{volatile auto r=cppunivariate::ztbinom_mle({x.data(),x.size()});});run("zmbinom",[&]{volatile auto r=cppunivariate::zmbinom_mle({z.data(),z.size()});});run("zib",[&]{volatile auto r=cppunivariate::zib_mle({z.data(),z.size()});});run("tbinom",[&]{volatile auto r=cppunivariate::tbinom_design_mle({x.data(),x.size()},0,10,NAN);});}
