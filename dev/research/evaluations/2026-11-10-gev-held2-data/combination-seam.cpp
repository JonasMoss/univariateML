#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
#include "cppunivariate/distributions/gev/dpqr.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <fstream>
std::vector<double> sample(double shape) {
  std::vector<double> x;
  for(int j=0;j<80;++j)x.push_back(cppunivariate::distributions::qgev((j+.5)/80,0,1,shape,true,false));
  return x;
}
int main(int argc,char** argv) {
  std::ofstream roots(argc>2?argv[2]:"build/scratch/seam-roots.jsonl");roots<<std::setprecision(17);
  const int maxit=argc>1?std::stoi(argv[1]):100;
  std::cout<<"target,shape,proposal_xi,proposal_score,status,certified,lambda,mu,sigma,loglik,budget,tie,maxit,milliseconds\n"<<std::setprecision(17);
  for(double target:{1e-4,1e-5,1e-6,1e-7}) {
    double lo=0,hi=.1;
    for(int k=0;k<60;++k) {
      const double mid=(lo+hi)/2;const auto x=sample(mid);
      const auto p=cppunivariate::gev_profile({x.data(),x.size()},0,1e-12,100);
      if(p.score<target)lo=mid;else hi=mid;
    }
    const double shape=(lo+hi)/2;const auto x=sample(shape);const cppunivariate::span z{x.data(),x.size()};
    const auto p0=cppunivariate::gev_profile(z,0,1e-12,100);
    const double xi=-p0.score/p0.curvature;
    const auto p=cppunivariate::gev_profile(z,xi,1e-12,100);
    const auto start=std::chrono::steady_clock::now();
    const auto r=cppunivariate::frechet_mle(z,1e-10,maxit);
    const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
    roots<<"{\"sample_id\":"<<int(-std::log10(target))<<",\"family\":\"frechet\",\"hold\":\"none\",\"status\":"<<int(r.status)<<",\"certified\":"<<r.certified<<",\"coef\":["<<r.lambda<<','<<r.mu<<','<<r.sigma<<"],\"data\":[";
    for(std::size_t j=0;j<x.size();++j)roots<<(j?",":"")<<x[j];roots<<"]}\n";
    std::cout<<target<<','<<shape<<','<<xi<<','<<p.score<<','<<int(r.status)<<','<<r.certified<<','<<r.lambda<<','<<r.mu<<','<<r.sigma<<','<<r.logLik<<','<<r.stop_detail.budget_exhausted<<','<<r.stop_detail.comparison_tie<<','<<maxit<<','<<ms<<std::endl;
  }
}
