// Reproducible fitted-nuisance model coverage, one stream per replication.
#include "cppunivariate/core/inference_families.hpp"
#include "cppunivariate/distributions/snorm/mle.hpp"
#include "cppunivariate/distributions/snorm/dpqr.hpp"
#include "cppunivariate/distributions/sstd/dpqr.hpp"
#include "cppunivariate/distributions/sstd/mle.hpp"
#include "cppunivariate/distributions/huber/dpqr.hpp"
#include "cppunivariate/distributions/pig/mle.hpp"
#include "cppunivariate/distributions/pig/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/gpd/mle.hpp"
#include "cppunivariate/distributions/gev/mle.hpp"
#include "cppunivariate/distributions/gpd/dpqr.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main(int argc,char** argv) {
 if(argc!=6)return 2;
 const std::string f=argv[1];const int n=std::stoi(argv[2]),R=std::stoi(argv[3]),first=std::stoi(argv[4]),tag=std::stoi(argv[5]);
 std::cout<<std::setprecision(17);
 for(int rep=first;rep<R;++rep) {
  cppunivariate::distributions::StdRng<> rng(20261107ULL+100000ULL*tag+1000ULL*n+rep);
  std::vector<double> x(n);std::vector<int> k(n);
  const bool sym=tag==1 || tag==3;
  const double skew=sym?1:1.5;
  for(int i=0;i<n;++i) {
   if(f=="snorm")x[i]=cppunivariate::distributions::rsnorm(rng,0,1,skew);
   else if(f=="sstd")x[i]=cppunivariate::distributions::rsstd(rng,0,1,5,skew);
   else if(f=="huber")x[i]=cppunivariate::distributions::rhuber(rng,0,1,1);
   else if(f=="pig")x[i]=k[i]=static_cast<int>(cppunivariate::distributions::rpig(rng,2,3));
   else if(f=="gpd")x[i]=cppunivariate::distributions::rgpd(rng,0,1,tag==6?-.25:tag==7?0:.25);
   else if(f=="gev")x[i]=cppunivariate::distributions::rgev(rng,0,1,tag==9?-.25:tag==10?0:.25);
   else return 3;
  }
  const cppunivariate::span data{x.data(),x.size()};
  std::array<double,8> p{};unsigned mask=7;int fit_status=-1;
  if(f=="snorm") {auto r=cppunivariate::snorm_mle(data,1e-10,2000);p[0]=r.mu;p[1]=r.sigma;p[2]=r.xi;fit_status=int(r.status);}
  if(f=="sstd") {auto r=cppunivariate::sstd_mle(data,1e-10,2000);p[0]=r.mu;p[1]=r.sigma;p[2]=r.nu;p[3]=r.xi;mask=15;fit_status=int(r.status);}
  if(f=="huber") {auto r=cppunivariate::huber_mle(data,1e-10,2000,NAN);p[0]=r.mu;p[1]=r.sigma;p[2]=r.epsilon;fit_status=int(r.status);}
  if(f=="pig") {auto r=cppunivariate::pig_mle({k.data(),k.size()},1e-10,2000);p[0]=r.mean;p[1]=r.shape;mask=3;fit_status=int(r.status);}
  if(f=="gpd") {auto r=cppunivariate::gpd_mle(data,1e-10,2000);p[0]=r.mu;p[1]=r.sigma;p[2]=r.xi;mask=6;fit_status=int(r.status);}
  if(f=="gev") {auto r=cppunivariate::gev_mle(data,1e-10,2000);p[0]=r.mu;p[1]=r.sigma;p[2]=r.xi;fit_status=int(r.status);}
  if(fit_status!=0){std::cout<<rep<<" fit_failure "<<fit_status<<"\n"<<std::flush;continue;}
  auto info=cppunivariate::family_inference(data,f,p,mask);
  auto a=cppunivariate::inference_covariance(info,cppunivariate::CovarianceKind::Observed);
  auto b=cppunivariate::inference_covariance(info,cppunivariate::CovarianceKind::Sandwich);
  if(info.status!=cppunivariate::InferenceStatus::Ok || a.status!=cppunivariate::InferenceStatus::Ok || b.status!=cppunivariate::InferenceStatus::Ok) {
   std::cout<<rep<<" inference_failure "<<int(info.status)<<" "<<int(a.status)<<" "<<int(b.status)<<"\n"<<std::flush;continue;
  }
  std::cout<<rep<<" ok";
  for(std::size_t j=0;j<info.dimension;++j)std::cout<<" "<<p[info.free_indices[j]]<<" "<<std::sqrt(a.matrix[j*info.dimension+j])<<" "<<std::sqrt(b.matrix[j*info.dimension+j]);
  std::cout<<"\n"<<std::flush;
 }
}
