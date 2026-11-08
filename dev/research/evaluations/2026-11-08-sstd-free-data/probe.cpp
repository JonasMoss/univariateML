#include "cppunivariate/distributions/sstd/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){for(const auto& x:std::vector<std::vector<double>>{{-2,-1,-.4,0,.2,.3,.5,1,2,5},{.5,1,1.5,2,2.5,3,10},{-1,1},{-3,-1,-.5,0,.1,.2,.4,1,2,4}})for(double nu:{2.1,3.,5.,20.}){auto r=cppunivariate::sstd_mle({x.data(),x.size()},1e-6,200,0,1,nu);std::cout<<std::setprecision(17)<<x.size()<<" "<<nu<<" "<<int(r.status)<<" "<<r.certified<<" "<<r.xi<<" "<<r.logLik<<" "<<r.iterations<<" "<<r.stop_detail.comparison_tie<<" "<<r.stop_detail.budget_exhausted<<"\n"; for(const auto& c:r.stop_detail.candidates){std::cout<<" candidate "<<int(c.model);for(double v:c.coef)std::cout<<" "<<v;std::cout<<" "<<c.loglik_lower<<" "<<c.loglik_upper<<"\n";}}}
