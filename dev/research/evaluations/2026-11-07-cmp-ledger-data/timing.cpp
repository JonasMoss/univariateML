#include "json_loader.hpp"
#include "common/distributions/cmp/fit.hpp"
#include "common/distributions/cmb/fit.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#ifndef PHASE
#define PHASE "after"
#endif
int main() {
 std::cout<<std::setprecision(17);
 for(const std::string id:{"cmp","cmb"}) {
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/"+id+"/golden.json");
  std::vector<double> data;for(int k=0;k<500;++k)data.push_back(g.data[k%g.data.size()]);
  std::vector<int> ix;for(double k:data)ix.push_back(int(k));
  for(int repeat=0;repeat<3;++repeat)for(int mode=0;mode<(id=="cmb"?3:1);++mode) {
   const auto start=std::chrono::steady_clock::now();
   const auto r=id=="cmp"?umvl_binding::fit_cmp({data.data(),data.size()}):
    umvl_binding::fit_cmb({ix.data(),ix.size()},{mode==0,g.estimates.at("size")},{},{mode==2,g.estimates.at("nu")});
   std::cout<<PHASE<<'\t'<<id<<'\t'<<mode<<'\t'<<repeat<<'\t'
    <<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<'\t'<<r.logLik<<'\t'<<r.error<<std::endl;
  }
 }
}
