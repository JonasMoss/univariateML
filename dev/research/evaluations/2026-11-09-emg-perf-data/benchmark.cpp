#include "cppunivariate/distributions/emg/mle.hpp"
#include "json_loader.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
#include <random>
#include <ctime>
// The work column is EmgResult::iterations: mask 6 uses location solver
// iterations, not the outer certificate ledger (which is inapplicable there).
int main(int argc,char** argv) {
  const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/emg/golden.json");
  const double tol=argc>1?std::stod(argv[1]):1e-8;
  const int selected_mask=argc>2?std::stoi(argv[2]):-1,selected_n=argc>3?std::stoi(argv[3]):0;
  std::cout<<std::unitbuf<<std::setprecision(17)<<"n,mask,tol,status,mu,sigma,lambda,logLik,certificate_evaluations,profiles,winners,seconds,cpu_seconds\n";
  for(int repeats:{1,20}) {
    std::vector<double> data=g.data;
    if(repeats==20) {
      data.clear();std::mt19937_64 rng(162);
      auto uniform=[&](){return (static_cast<double>(rng()>>11)+.5)*0x1p-53;};
      for(int i=0;i<10000;++i) {
        const double u1=uniform(),u2=uniform(),u3=uniform();
        const double z=std::sqrt(-2*std::log(u1))*std::cos(2*std::acos(-1.)*u2);
        data.push_back(g.estimates.at("mu")+g.estimates.at("sigma")*z-std::log(u3)/g.estimates.at("lambda"));
      }
    }
    if(selected_n&&selected_n!=static_cast<int>(data.size()))continue;
    for(int mask=0;mask<7;++mask) {
      if(selected_mask>=0&&mask!=selected_mask)continue;
      const cppunivariate::span x{data.data(),data.size()};
      const double mu=mask&1?g.estimates.at("mu"):NAN,sigma=mask&2?g.estimates.at("sigma"):NAN,lambda=mask&4?g.estimates.at("lambda"):NAN;
#ifdef EMG_PERF_PROFILE
      cppunivariate::emg_detail::perf_evaluations=cppunivariate::emg_detail::perf_hessians=cppunivariate::emg_detail::perf_newton_calls=cppunivariate::emg_detail::perf_newton_steps=cppunivariate::emg_detail::perf_fallbacks=cppunivariate::emg_detail::perf_depth=0;
#endif
      const auto cpu_start=std::clock();
      const auto start=std::chrono::steady_clock::now();
      cppunivariate::EmgResult r;std::size_t profiles=0,winners=0;
      if(mask==6)r=cppunivariate::emg_mle(x,tol,500,mu,sigma,lambda);
      else {cppunivariate::emg_detail::OuterCertificate c(x,tol,500,mu,sigma,lambda);r=c.run();profiles=c.cache.size();winners=c.winners.size();}
      const double seconds=std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
      const double cpu_seconds=static_cast<double>(std::clock()-cpu_start)/CLOCKS_PER_SEC;
      std::cout<<data.size()<<','<<mask<<','<<tol<<','<<int(r.status)<<','<<r.mu<<','<<r.sigma<<','<<r.lambda<<','<<r.logLik<<','<<r.iterations<<','<<profiles<<','<<winners<<','<<seconds<<','<<cpu_seconds<<'\n';
#ifdef EMG_PERF_PROFILE
      std::cerr<<data.size()<<','<<mask<<','<<cppunivariate::emg_detail::perf_evaluations<<','<<cppunivariate::emg_detail::perf_hessians<<','<<cppunivariate::emg_detail::perf_newton_calls<<','<<cppunivariate::emg_detail::perf_newton_steps<<','<<cppunivariate::emg_detail::perf_fallbacks<<','<<cppunivariate::emg_detail::perf_depth<<'\n';
#endif
    }
  }
}
