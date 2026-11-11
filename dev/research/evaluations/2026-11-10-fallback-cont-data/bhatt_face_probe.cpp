#include "cppunivariate/distributions/bhatt/mle.hpp"
#include <iostream>
#include <iomanip>
std::vector<double> sample(double t){std::vector<double> x{-1,1};for(int i=0;i<10;++i){x.push_back(-t);x.push_back(t);}return x;}
int main(){double lo=0,hi=1;for(int i=0;i<45;++i){double t=(lo+hi)/2;auto x=sample(t);auto r=cppunivariate::bhatt_detail::legacy_candidate({x.data(),x.size()},1e-14,1000,NAN,NAN,NAN,true);if(r.status==cppunivariate::BhattStatus::Ok&&r.logLik>-22*std::log(2.))lo=t;else hi=t;}
std::cout<<std::setprecision(17)<<lo<<' '<<hi<<'\n';for(double e:{-1e-6,-1e-8,-1e-10,0.,1e-10}){double t=lo+e;auto x=sample(t);cppunivariate::span d{x.data(),x.size()};auto r=cppunivariate::bhatt_detail::legacy_candidate(d,1e-14,1000);auto c=cppunivariate::bhatt_detail::certificate_mle(d,1e-14,1000,NAN,NAN,NAN);auto f=cppunivariate::bhatt_mle(d,1e-14,1000);auto a=cppunivariate::bhatt_detail::legacy_candidate(d,1e-14,1000,NAN,NAN,NAN,true);std::cout<<t<<' '<<int(r.status)<<' '<<int(c.status)<<' '<<int(f.status)<<' '<<f.certified<<' '<<int(a.status)<<' '<<a.logLik+22*std::log(2.)<<'\n';}}
