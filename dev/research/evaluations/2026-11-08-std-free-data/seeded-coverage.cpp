// Reproduce from the repository root; stdout is seeded-coverage.csv.
#include "cppunivariate/distributions/std/mle.hpp"
#include "cppunivariate/distributions/norm/dpqr.hpp"
#include "cppunivariate/distributions/lst/dpqr.hpp"
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <random>
#include <string>
int main(){
  constexpr int repetitions=32,n=64,maxit=200;
  constexpr double tol=1e-10;
  const std::vector<std::string> sources{"normal","t3","t5","t10","t30","uniform","triangular"};
  const std::vector<double> dfs{INFINITY,3,5,10,30,INFINITY,INFINITY};
  std::ofstream inputs("dev/research/evaluations/2026-11-08-std-free-data/seeded-samples.csv");
  if(!inputs)return 2;
  inputs<<std::setprecision(17);std::cout<<std::setprecision(17);
  inputs<<"family,source,seed";for(int i=0;i<n;++i)inputs<<",x"<<i;inputs<<"\n";
  std::cout<<"family,source,seed,held_mask,status,certified,category,seconds,mu,physical_scale,shape,loglik,iterations\n";
  for(std::size_t source=0;source<sources.size();++source){
    for(int seed=1;seed<=repetitions;++seed){
      std::mt19937_64 rng(1164500+1000*source+seed);
      const auto uniform=[&](){return (static_cast<double>(rng()>>12)+.5)*0x1p-52;};
      std::vector<double> sample;
      for(int j=0;j<n;++j){
        const double u=uniform();double z=0;
        if(source==0)z=cppunivariate::distributions::qnorm(u,0,1,true,false);
        else if(source<=4)z=cppunivariate::distributions::qlst(u,0,1,dfs[source],true,false);
        else if(source==5)z=(2*u-1)*std::sqrt(3.);
        else z=(u-uniform())*std::sqrt(6.);
        sample.push_back(z);
      }
      for(bool std_family:{false,true}){
        auto x=sample;
        if(std_family&&source>=1&&source<=4)for(auto& z:x)z*=std::sqrt((dfs[source]-2)/dfs[source]);
        const std::string family=std_family?"std":"lst";
        inputs<<family<<","<<sources[source]<<","<<seed;for(double z:x)inputs<<","<<z;inputs<<"\n";
        for(int mask:{3,1,2}){
          const double mu=mask&1?0:NAN,scale=mask&2?1:NAN;
          int status=0,iterations=0;bool certified=false,finite=false,face=false;
          double m=NAN,s=NAN,v=NAN,ll=NAN;
          const auto begin=std::chrono::steady_clock::now();
          if(std_family){
            const auto fit=cppunivariate::student_t_mle({x.data(),x.size()},tol,maxit,mu,scale,NAN);
            status=static_cast<int>(fit.status);iterations=fit.iterations;
            finite=fit.status==cppunivariate::StudentTStatus::Ok;
            face=fit.status==cppunivariate::StudentTStatus::NormalBoundary||fit.status==cppunivariate::StudentTStatus::DfTwoBoundary||fit.status==cppunivariate::StudentTStatus::ScaleBoundary;
            certified=fit.certified&&(finite||face);m=fit.mu;s=fit.sigma;v=fit.nu;ll=fit.logLik;
          }else{
            const auto fit=cppunivariate::lst_mle({x.data(),x.size()},tol,maxit,mu,scale,NAN);
            status=static_cast<int>(fit.status);iterations=fit.iterations;
            finite=fit.status==cppunivariate::LstStatus::Ok;
            face=fit.status==cppunivariate::LstStatus::NormalBoundary||fit.status==cppunivariate::LstStatus::ScaleBoundary;
            certified=fit.certified&&(finite||face);m=fit.mu;s=fit.sigma;v=fit.df;ll=fit.logLik;
          }
          const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-begin).count();
          const char* category=finite?(certified?"certified_finite":"uncertified_finite"):
            face?(certified?"certified_face":"uncertified_face"):"stop";
          std::cout<<family<<","<<sources[source]<<","<<seed<<","<<mask<<","<<status<<","<<certified<<","<<category<<","<<seconds<<","<<m<<","<<s<<","<<v<<","<<ll<<","<<iterations<<"\n";
        }
      }
    }
    std::cerr<<"finished "<<sources[source]<<" (32 seeds, both families, all three subsets)\n";
  }
  return inputs&&std::cout?0:3;
}
