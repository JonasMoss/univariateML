#include "common/distributions/ztbinom/fit.hpp"
#include "common/distributions/zmbinom/fit.hpp"
#include "common/distributions/zib/fit.hpp"
#include "common/distributions/tbinom/fit.hpp"
#include <iostream>
void print(const std::string &name,const umvl_binding::FitResult &f) {
 std::cout<<name<<" kind="<<f.error_kind<<" model="<<f.model<<" upper="<<f.loglik_upper<<" candidates="<<f.candidates.size()<<" error="<<f.error<<'\n';
}
int main(){
 const std::vector<int> a{1,1,1,2,3,7,12}, b{0,0,1,1,1,2,3,7,12}, c{0,0,0,0,0,0,0,0,1,1,1,2,3,7,12},d{1,2,3};
 print("ztbinom",umvl_binding::fit_ztbinom({a.data(),a.size()}));
 print("zmbinom",umvl_binding::fit_zmbinom({b.data(),b.size()},{},{},{true,.2}));
 print("zib",umvl_binding::fit_zib({c.data(),c.size()},{},{},{true,.2}));
 print("tbinom",umvl_binding::fit_tbinom({d.data(),d.size()}));
}
