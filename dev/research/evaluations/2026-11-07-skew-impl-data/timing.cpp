#include "cppunivariate/distributions/sstd/mle.hpp"
#include "cppunivariate/distributions/st/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main(){
 std::cout<<std::setprecision(17);
 for(int n:{7,500}){
 std::vector<double> x{-3,-1,-.2,.1,.8,2,4};
 if(n==500){x.clear();for(int i=0;i<n;++i){double z=std::tan(2.5*((i+.5)/n-.5));x.push_back(z>0?1.5*z:z);}}
 const cppunivariate::span s{x.data(),x.size()}; const double nan=NAN;
 for(int j=0;j<3;++j){auto start=std::chrono::steady_clock::now();double ll=0,p=0;int status=0;
 for(int k=0;k<10;++k){if(j<2){auto r=cppunivariate::sstd_mle(s,1e-6,200,j==0?nan:0,j==0?1:nan,3,2);ll=r.logLik;p=j==0?r.mu:r.sigma;status=int(r.status);}else{auto r=cppunivariate::st_mle(s,1e-6,200,0,1,nan,4);ll=r.logLik;p=r.alpha;status=int(r.status);}}
 std::cout<<"n "<<n<<" route "<<j<<" parameter "<<p<<" ll "<<ll<<" status "<<status<<" ms_per_fit "<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/10<<"\n";
 }
 }
}
