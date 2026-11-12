#include "common/distributions/zib/fit.hpp"
#include "common/distributions/foldnorm/fit.hpp"
#include <iostream>
#include <iomanip>
int main(){
 int x[]={0,1,1};
 auto a=umvl_binding::fit_zib({x,3},{true,1},{},{true,0},1e-20,200);
 double y[]={1,1,1,std::sqrt(9+4*std::sqrt(6.))};
 auto b=umvl_binding::fit_foldnorm({y,4});
 std::cout<<std::setprecision(17)<<a.error_kind<<" "<<a.attainable_tol<<" "<<a.candidates.size()<<" "<<a.error<<"\n";
 std::cout<<b.error_kind<<" "<<b.attainable_tol<<" "<<b.candidates.size()<<" "<<b.error<<"\n";
}
