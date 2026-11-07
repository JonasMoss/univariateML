#include "cppunivariate/distributions/std/mle.hpp"
#include <chrono>
#include <iostream>
int main(){std::vector<double>x;for(int j=0;j<500;++j)x.push_back(std::sin(j*1.71)+.3*std::cos(j*.41));auto start=std::chrono::steady_clock::now();for(int mask=1;mask<7;++mask){auto t=std::chrono::steady_clock::now();auto r=cppunivariate::lst_mle({x.data(),x.size()},1.220703e-4,200,mask&1?.2:NAN,mask&2?1.3:NAN,mask&4?3:NAN);auto s=cppunivariate::student_t_mle({x.data(),x.size()},1.220703e-4,200,mask&1?.2:NAN,mask&2?1.3:NAN,mask&4?3:NAN);std::cout<<mask<<" "<<int(r.status)<<" "<<int(s.status)<<" "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-t).count()<<"\n";} }
