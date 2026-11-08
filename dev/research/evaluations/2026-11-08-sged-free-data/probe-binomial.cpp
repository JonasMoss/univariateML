// Isolated baseline: these unchanged headers do not include SGED.
#include "common/distributions/ztbinom/fit.hpp"
#include "common/distributions/zmbinom/fit.hpp"
#include "common/distributions/zib/fit.hpp"
#include "common/distributions/tbinom/fit.hpp"
#include <iomanip>
#include <iostream>
#include <vector>
int main() {
  const std::vector<int> a{1,1,1,2,3,7,12},b{0,0,1,1,1,2,3,7,12},c{0,0,0,0,0,0,0,0,1,1,1,2,3,7,12},d{1,2,3};
  const auto print=[](const std::string& family,const umvl_binding::FitResult& r) {
    std::cout<<std::setprecision(17)<<family<<'\t'<<r.error_kind<<'\t'<<r.model<<'\t'<<r.loglik_lower<<'\t'<<r.loglik_upper<<'\t'<<r.candidates.size()<<'\n';
  };
  print("ztbinom",umvl_binding::fit_ztbinom({a.data(),a.size()}));
  print("zmbinom",umvl_binding::fit_zmbinom({b.data(),b.size()},{},{},{true,.2}));
  print("zib",umvl_binding::fit_zib({c.data(),c.size()},{},{},{true,.2}));
  print("tbinom",umvl_binding::fit_tbinom({d.data(),d.size()}));
}
