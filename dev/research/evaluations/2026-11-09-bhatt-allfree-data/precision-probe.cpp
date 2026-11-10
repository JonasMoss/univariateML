#include "cppunivariate/distributions/bhatt/mle.hpp"
#include <iostream>
#include <iomanip>
int main(){std::cout<<std::setprecision(17);for(const std::vector<double> x:{std::vector<double>{-3,-1,0,1,2},std::vector<double>{-3,-1,0,1,3}}){auto f=cppunivariate::bhatt_detail::legacy_candidate({x.data(),x.size()},1e-30,500,1,2);cppunivariate::bhatt_detail::CertModel m({x.data(),x.size()},NAN,1,2);std::array<double,3> p{{f.mu,0,0}};auto e=cppunivariate::bhatt_detail::cert_evaluate(m,p);std::cout<<int(f.status)<<' '<<f.mu<<' '<<e.gradient[0].lo<<' '<<e.gradient[0].hi<<'\n';}}
