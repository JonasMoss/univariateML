#include "json_loader.hpp"
#include "common/distributions/cmb/fit.hpp"
#include <iostream>
#include <chrono>
int main(){
 auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/cmb/golden.json");
 std::vector<int> x;for(double v:g.data)x.push_back(int(v));
 for(int mask=0;mask<4;++mask){auto r=umvl_binding::fit_cmb({x.data(),x.size()},{true,g.estimates.at("size")},{bool(mask&1),g.estimates.at("prob")},{bool(mask&2),g.estimates.at("nu")});std::cout<<mask<<'\t'<<r.error_kind<<'\t'<<r.error<<'\t'<<r.logLik<<std::endl;}
 std::vector<int> big;for(int i=0;i<100000;++i)big.push_back(x[i%x.size()]);auto start=std::chrono::steady_clock::now();auto r=umvl_binding::fit_cmb({big.data(),big.size()},{},{},{true,g.estimates.at("nu")});std::cout<<"large-held-nu\t"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<'\t'<<r.error_kind<<'\t'<<r.error<<'\t'<<r.candidates.size()<<std::endl;
 std::vector<int> small{0,1};for(int i:{1,2}){auto f=umvl_binding::fit_cmb({small.data(),small.size()},{true,100001},{true,.5},{true,1},1e-10,i);std::cout<<"preflight\t"<<i<<'\t'<<f.error_kind<<'\t'<<f.error<<'\t'<<f.logLik<<std::endl;}
}
