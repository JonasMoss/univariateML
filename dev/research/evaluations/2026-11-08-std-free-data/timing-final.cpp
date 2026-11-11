#include "cppunivariate/distributions/std/mle.hpp"
#include "cppunivariate/distributions/lst/dpqr.hpp"
#include <chrono>
#include <iostream>
int main(){
 std::vector<double> x;for(int i=0;i<500;++i)x.push_back(cppunivariate::distributions::qlst((i+.5)/500.,0,1,5,true,false));
 for(int repeat=0;repeat<3;++repeat)for(int family=0;family<2;++family)for(int subset=0;subset<3;++subset){
  const double m=subset==2?NAN:0,s=subset==1?NAN:1.3;
  auto start=std::chrono::steady_clock::now();
  if(family==0){auto r=cppunivariate::student_t_mle({x.data(),x.size()},1e-8,200,m,s,NAN);std::cout<<"std,"<<subset<<","<<static_cast<int>(r.status)<<","<<r.mu<<","<<r.sigma<<","<<r.nu;}
  else {auto r=cppunivariate::lst_mle({x.data(),x.size()},1e-8,200,m,s,NAN);std::cout<<"lst,"<<subset<<","<<static_cast<int>(r.status)<<","<<r.mu<<","<<r.sigma<<","<<r.df;}
  std::cout<<","<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<"\n";
 }

#ifdef UMVL_TIMING_BASELINE
 const int modes=1;
#else
 const int modes=3;
#endif
 for(int mode=0;mode<modes;++mode){
  const std::vector<double> data=mode==0?x:mode==1?std::vector<double>{-100,100}:std::vector<double>{-100,-99,0,99,100};
  auto start=std::chrono::steady_clock::now();int status=0;
  for(int i=0;i<1000;++i){
   if(mode==1){auto r=cppunivariate::student_t_mle({data.data(),data.size()},1e-8,200,NAN,1,NAN);status=static_cast<int>(r.status);}
   else{auto r=cppunivariate::lst_mle_fixed_df({data.data(),data.size()},.5,1e-8,200);status=static_cast<int>(r.status);}
  }
  std::cout<<(mode==1?"std":"lst")<<","<<(mode==0?"joint-at-df-0.5":mode==1?"held-sd-symmetry":"joint-subunit-symmetry")<<","<<status<<",nan,nan,nan,"<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()/1000<<"\n";
 }
}
