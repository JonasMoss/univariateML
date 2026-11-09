#include "cppunivariate/distributions/gb2/mle.hpp"
#include "cppunivariate/distributions/fpareto/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main(){
 std::vector<double> x{.03,.1,.3,.7,1,2,5,20};
 std::cout<<std::setprecision(17);
 for(int family=0;family<2;++family)for(int subset=0;subset<3;++subset){
  auto y=x;if(family)for(auto&v:y)v+=2;
  auto start=std::chrono::steady_clock::now();
  for(int rep=0;rep<50;++rep){
   double a=subset==2?2:NAN,b=subset==1?1:NAN;
   cppunivariate::Gb2Result r{};
   if(!family)r=cppunivariate::gb2_mle({x.data(),x.size()},1e-7,300,2,a,3,b);
   if(family){auto s=cppunivariate::fpareto_mle({y.data(),y.size()},2,1e-7,300,2,a,3,b);r={s.shape1,s.shape2,s.shape3,s.scale,s.logLik,s.iterations,s.status,s.stop_detail};}
   if(rep==0)std::cout<<family<<' '<<subset<<' '<<int(r.status)<<' '<<r.shape2<<' '<<r.scale<<' '<<r.logLik<<' '<<r.stop_detail.loglik_lower<<' '<<r.stop_detail.loglik_upper<<' ';
  }
  std::cout<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/50<<" ms\n";
 }
 // Mathematical free sets: pa, qa, pqb, pqa, pab, qab, pqab.
 const int masks[]={6,3,13,7,14,11,15};
 for(int family=0;family<2;++family)for(int mask:masks){
  auto y=x;if(family)for(auto&v:y)v+=2;
  const auto start=std::chrono::steady_clock::now();
  for(int rep=0;rep<3;++rep){
   const double q=mask&1?NAN:2,a=mask&2?NAN:2,p=mask&4?NAN:3,b=mask&8?NAN:1;
   cppunivariate::Gb2Result r{};
   if(!family)r=cppunivariate::gb2_mle({x.data(),x.size()},1e-7,300,q,a,p,b);
   else {auto s=cppunivariate::fpareto_mle({y.data(),y.size()},2,1e-7,300,q,a,p,b);r={s.shape1,s.shape2,s.shape3,s.scale,s.logLik,s.iterations,s.status,s.stop_detail};}
   if(rep==0)std::cout<<"mixed "<<family<<' '<<mask<<' '<<int(r.status)<<' '<<r.shape1<<' '<<r.shape2<<' '<<r.shape3<<' '<<r.scale<<' '<<r.logLik<<' '<<r.stop_detail.loglik_lower<<' '<<r.stop_detail.loglik_upper<<' '<<r.iterations<<' ';
  }
  std::cout<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/3<<" ms\n";
 }

 for(int family=0;family<2;++family)for(bool inverse:{false,true}){
  std::vector<double> y{.1,.2,.4,.8};for(double&v:y){if(inverse)v=1/v;if(family)v+=2;}
  const auto start=std::chrono::steady_clock::now();
  for(int rep=0;rep<3;++rep){
   cppunivariate::Gb2Result r{};
   if(!family)r=cppunivariate::gb2_mle({y.data(),y.size()},1e-7,300,inverse?NAN:.4,NAN,inverse?.4:NAN,1);
   else {auto s=cppunivariate::fpareto_mle({y.data(),y.size()},2,1e-7,300,inverse?NAN:.4,NAN,inverse?.4:NAN,1);r={s.shape1,s.shape2,s.shape3,s.scale,s.logLik,s.iterations,s.status,s.stop_detail};}
   if(rep==0)std::cout<<"endpoint "<<family<<' '<<inverse<<' '<<int(r.status)<<' '<<r.logLik<<' ';
  }
  std::cout<<std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count()/3<<" ms\n";
 }

 const std::vector<double> constant(5,2);
 const auto corner=cppunivariate::gb2_mle({constant.data(),constant.size()},1e-7,300,10,1e-300,std::nextafter(10.,INFINITY),NAN);
 std::cout<<"constant "<<int(corner.status)<<' '<<corner.scale<<' '<<corner.logLik<<'\n';

 const std::vector<double> repeated(5,.5);
 const auto finite=cppunivariate::gb2_mle({repeated.data(),repeated.size()},1e-7,300,2,NAN,NAN,1);
 std::cout<<"constant_finite "<<int(finite.status)<<' '<<finite.shape2<<' '<<finite.shape3<<' '<<finite.logLik<<' '<<finite.stop_detail.loglik_lower<<' '<<finite.stop_detail.loglik_upper<<'\n';

}
