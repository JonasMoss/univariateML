#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <chrono>
#include <iostream>
#include <iomanip>
int main() {
  std::cout<<std::setprecision(17)<<"[\n";
  int row=0;
  for(const std::vector<int> &x:{std::vector<int>{0,0,5,5},std::vector<int>{0,2,2,2},std::vector<int>{0,1,2,3,4,5}}) {
    cppunivariate::bnbinom_detail::Data d;std::map<int,double> counts;
    for(int v:x)counts[v]++;
    d.n=x.size();for(const auto &kv:counts){d.x.push_back(kv.first);d.count.push_back(kv.second);}
    const auto nb=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,600);
    const auto po=cppunivariate::pois_mle({x.data(),x.size()});
    const bool finite_nb=nb.status==cppunivariate::NbinomStatus::Ok;
    const double r=finite_nb?nb.size:NAN;
    const double pm=cppunivariate::bnbinom_detail::joint_error(d,po.logLik,std::abs(po.logLik))+
      1e-9*d.n*(1+std::abs(po.logLik/d.n));
    const auto bound=finite_nb?cppunivariate::bnbinom_detail::joint_nb_profile_bound(d,r):
      cppunivariate::bnbinom_detail::AlphaLikelihood{po.logLik,pm};
    for(int maxit:{600,6000}) {
      const auto start=std::chrono::steady_clock::now();
      const auto proof=cppunivariate::bnbinom_detail::signed_face_cover(d,r,bound.value-bound.margin,po.logLik+pm,maxit);
      const double ms=std::chrono::duration<double,std::milli>(std::chrono::steady_clock::now()-start).count();
      if(row++)std::cout<<",\n";
      std::cout<<"{\"sample\":\""<<(x.back()==5?(x.size()==4?"zero-five":"zero-through-five"):"zero-two-two-two")
        <<"\",\"maxit\":"<<maxit<<",\"proved\":"<<(proof.proved?"true":"false")
        <<",\"budget\":"<<(proof.budget_exhausted?"true":"false")<<",\"evaluations\":"<<proof.evaluations
        <<",\"size_upper\":"<<proof.size_upper<<",\"concentration_cutoff\":";
      if(std::isfinite(proof.concentration_upper))std::cout<<proof.concentration_upper;else std::cout<<"null";
      std::cout<<",\"milliseconds\":"<<ms<<"}";
    }
  }
  std::cout<<"\n]\n";
}
