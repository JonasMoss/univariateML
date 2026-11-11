#include <iostream>
#include <iomanip>
#include <random>
#include "cppunivariate/distributions/ht/mle.hpp"
int main(){std::cout<<std::setprecision(17);std::mt19937_64 rng(1162026);for(int i=0;i<600;++i){const int group=i/200;std::student_t_distribution<double>td(group==0?.4:group==1?80.:3.);std::vector<double>x(24);for(double&v:x)v=std::abs(td(rng));if(group==2){x[0]=0;x[1]=0;}if(i==206||i==226||i==227||i==282||i==363){auto old=cppunivariate::ht_accuracy_search({x.data(),x.size()},0,NAN,NAN);auto r=cppunivariate::ht_mle({x.data(),x.size()},0,NAN,NAN);std::cout<<"{\"index\":"<<i<<",\"nu\":"<<r.nu<<",\"sigma\":"<<r.sigma<<",\"status\":"<<int(r.status)<<",\"before\":["<<old.nu<<","<<old.sigma<<"],\"data\":[";for(int j=0;j<24;++j){if(j)std::cout<<",";std::cout<<x[j];}std::cout<<"]}"<<std::endl;}}}
