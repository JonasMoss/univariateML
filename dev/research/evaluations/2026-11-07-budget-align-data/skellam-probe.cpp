#include "common/distributions/skellam/fit.hpp"
#include <iostream>
int main(){for(double scale:{1e6,1e9,1e14}){std::vector<double> x{-scale,0,scale};auto r=umvl_binding::fit_skellam({x.data(),x.size()},{},{},1e-9,1);std::cout<<scale<<'\t'<<r.error_kind<<'\t'<<r.candidates.size()<<'\t'<<r.loglik_lower<<'\t'<<r.error<<std::endl;}}
