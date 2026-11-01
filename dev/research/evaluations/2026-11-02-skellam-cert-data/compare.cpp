#include "cppunivariate/distributions/skellam/mle.hpp"
#define SkellamStatus SkellamOldStatus
#define SkellamResult SkellamOldResult
#define skellam_mle skellam_old_mle
#include "old.hpp"
#undef SkellamStatus
#undef SkellamResult
#undef skellam_mle
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <random>
#include <vector>
int main() {
  std::mt19937 gen(116);
  const double rates[][2]={{.02,.02},{.03,3},{3,.03},{1,1},{8,4},{25,25},{0,4},{4,0},{.001,12},{12,.001},{.2,5},{5,.2}};
  int bad=0,fail=0; double maxdiff=0;
  std::ofstream samples("dev/research/evaluations/2026-11-02-skellam-cert-data/samples.csv");
  samples<<"sample,x,old_mu1,old_mu2,old_logLik\n";
  for(int i=0;i<600;++i) {
    const auto &r=rates[i%12];
    std::poisson_distribution<int> a(r[0]),b(r[1]);
    std::vector<double> x(20+i%61);
    for(double &v:x) v=a(gen)-b(gen);
    cppunivariate::span data{x.data(),x.size()};
    const auto old=cppunivariate::skellam_old_mle(data);
    const auto now=cppunivariate::skellam_mle(data);
    for(double v:x) samples<<i<<','<<v<<','<<std::setprecision(17)<<old.mu1<<','<<old.mu2<<','<<old.logLik<<'\n';
    if(old.status!=cppunivariate::SkellamOldStatus::Ok || now.status!=cppunivariate::SkellamStatus::Ok) {++fail;std::cout<<"failure "<<i<<' '<<static_cast<int>(old.status)<<' '<<static_cast<int>(now.status)<<'\n';continue;}
    const double diff=now.logLik-old.logLik;
    maxdiff=std::max(maxdiff,std::abs(diff));
    if(std::abs(diff)>1e-8*x.size() || std::abs(now.mu1-old.mu1)>1e-4*(1+old.mu1) || std::abs(now.mu2-old.mu2)>1e-4*(1+old.mu2)) {++bad;std::cout<<"disagreement "<<i<<' '<<diff<<' '<<old.mu1<<' '<<old.mu2<<' '<<now.mu1<<' '<<now.mu2<<'\n';}
  }
  std::cout<<"samples=600 failures="<<fail<<" disagreements="<<bad<<" max_ll_difference="<<maxdiff<<'\n';
  std::poisson_distribution<int> a(8),b(4);std::vector<double> x(500);for(double &v:x) v=a(gen)-b(gen);
  cppunivariate::span data{x.data(),x.size()};
  auto start=std::chrono::steady_clock::now();
  for(int i=0;i<5;++i) {auto r=cppunivariate::skellam_old_mle(data);if(r.status!=cppunivariate::SkellamOldStatus::Ok) return 2;}
  auto mid=std::chrono::steady_clock::now();
  for(int i=0;i<5;++i) {auto r=cppunivariate::skellam_mle(data);if(r.status!=cppunivariate::SkellamStatus::Ok) return 3;}
  auto end=std::chrono::steady_clock::now();
  const double old=std::chrono::duration<double>(mid-start).count(),now=std::chrono::duration<double>(end-mid).count();
  std::cout<<"n=500 repetitions=5 old_seconds="<<old<<" new_seconds="<<now<<" ratio="<<now/old<<'\n';
  return (bad || fail) ? 1:0;
}
