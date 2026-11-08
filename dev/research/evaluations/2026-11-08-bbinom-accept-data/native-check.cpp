#include "cppunivariate/distributions/bbinom/mle.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
int main() {
  int N,n; double a,b;
  std::cout<<std::setprecision(17);
  while(std::cin>>N>>a>>b>>n) {
    std::vector<int>x(n);for(int& v:x)std::cin>>v;
    cppunivariate::bbinom_detail::CellWork work(cppunivariate::enumeration_budget(500));
    const auto r=cppunivariate::bbinom_mle_work({x.data(),x.size()},N,1e-10,500,
      a>0?a:NAN,b>0?b:NAN,work);
    std::cout<<int(r.status)<<" "<<(std::isfinite(r.alpha)?r.alpha:0)<<" "
      <<(std::isfinite(r.beta)?r.beta:0)<<" "<<work.used<<"\n";
  }
}
