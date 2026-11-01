// Fixed-shape experiment. The production fit supplies the shapes; this does
// not validate an outer shape search. Seed and workload are recorded in report.
#include <cppunivariate/cppunivariate.hpp>
#include <cppunivariate/distributions/rng_std.hpp>
#include <cppunivariate/distributions/ged/dpqr.hpp>
#include <cppunivariate/distributions/sged/dpqr.hpp>
#include <chrono>
#include <iostream>
#include <iomanip>
#include <random>
#include <vector>
#include <string>
#include <algorithm>
#include <cmath>

int main(int argc,char**argv) {
  if(argc!=2)return 2;
  const std::string family=argv[1];
  cppunivariate::distributions::StdRng<> rng(11620261101ULL);
  std::cout<<std::setprecision(17)<<"sample,n,shape,skew,status,old_ll,inner_ll,fit_seconds,inner_seconds\n";
  for(int i=0;i<600;++i) {
    const int n=i<200?8:i<400?32:128;
    const double nu=std::vector<double>{1,1.2,2,4,8}[i%5];
    const double skew=std::vector<double>{0.5,1,2}[i%3];
    const double alpha=std::vector<double>{-8,-2,0,2,8}[i%5];
    std::vector<double>x; x.reserve(n);
    for(int j=0;j<n;++j) {
      if(family=="ged")x.push_back(cppunivariate::distributions::rged(rng,0,1,nu));
      else if(family=="sged")x.push_back(cppunivariate::distributions::rsged(rng,0,1,nu,skew));
      else x.push_back(cppunivariate::distributions::rsn(rng,0,1,alpha));
    }
    const auto t0=std::chrono::steady_clock::now();
    double ll=NAN, inner=NAN, shape=NAN, xi=NAN; int status=-1;
    if(family=="ged") {const auto r=cppunivariate::ged_mle({x.data(),x.size()},1e-8,1000);status=int(r.status);ll=r.logLik;shape=r.nu;}
    else if(family=="sged") {const auto r=cppunivariate::sged_mle({x.data(),x.size()},1e-8,1000);status=int(r.status);ll=r.logLik;shape=r.nu;xi=r.xi;}
    else {const auto r=cppunivariate::sn_mle({x.data(),x.size()},1e-8,1000);status=int(r.status);ll=r.logLik;shape=r.alpha;}
    const auto t1=std::chrono::steady_clock::now();
    if(status==0 && std::isfinite(shape)) {
      if(family=="ged") {
        const auto r=cppunivariate::ged_detail::conditional({x.data(),x.size()},shape,NAN,NAN);
        inner=r.logLik;
      } else if(family=="sn") {
        // Production result already comes from its own concave inner solver.
        inner=ll;
      } else if(shape>1 && std::isfinite(xi) && xi>0) {
        // Minimize the strictly convex side-weighted power loss in the mode;
        // the optimal inverse scale then follows from its power mean.
        auto mm=std::minmax_element(x.begin(),x.end()); double lo=*mm.first,hi=*mm.second;
        const double range=hi-lo;
        for(int k=0;k<100;++k) {
          double c=lo/2+hi/2, score=0;
          for(double v:x) {double d=(v-c)/range;score+=(d>0?1:-1)*std::pow(std::abs(d)/(d>0?xi:1/xi),shape-1)/(d>0?xi:1/xi);}
          if(score>0)lo=c; else hi=c;
        }
        const double mode=lo/2+hi/2;double power=0;
        for(double v:x){double d=(v-mode)/range;power+=std::pow(std::abs(d)/(d>0?xi:1/xi),shape);}
        double shift=0;const double su=cppunivariate::dens::sged_sigma(shape,xi,shift);
        const double sigma=range*std::pow(shape*power/(2*n),1/shape)*su/cppunivariate::dens::ged_lambda(shape);
        const double mean=mode+sigma*shift/su;
        inner=0;for(double v:x){const double z=(v-mean)*su/sigma+shift;inner+=std::log(su/sigma)+cppunivariate::dens::log_dsged_core(z,shape,xi);}
      }
    }
    const auto t2=std::chrono::steady_clock::now();
    std::cout<<i<<","<<n<<","<<shape<<","<<xi<<","<<status<<","<<ll<<","<<inner<<","<<std::chrono::duration<double>(t1-t0).count()<<","<<std::chrono::duration<double>(t2-t1).count()<<"\n";
  }
}
