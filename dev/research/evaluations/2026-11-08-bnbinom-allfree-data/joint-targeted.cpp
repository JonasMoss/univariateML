#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <fstream>
#include <iostream>
#include <iomanip>
int main(){std::ifstream input("build/scratch/bnbinom-allfree/counts.txt");std::vector<int>x;std::map<int,double> counts;int v;while(input>>v){x.push_back(v);counts[v]++;}
  cppunivariate::bnbinom_detail::Data d;d.n=x.size();for(auto kv:counts){d.x.push_back(kv.first);d.count.push_back(kv.second);}
  const auto nb=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,600);const auto po=cppunivariate::pois_mle({x.data(),x.size()});
  const auto p=cppunivariate::bnbinom_detail::joint_cover(d,{1.0672234787479615,2.1419791546447171,5.3647338351087397},nb.logLik,po.logLik,1e-8,600,nb.size);
  std::cout<<std::setprecision(17)<<"{\"proved\":"<<(p.proved?"true":"false")<<",\"budget\":"<<(p.budget_exhausted?"true":"false")<<",\"evaluations\":"<<p.evaluations<<",\"iterations\":"<<p.iterations;
  if(p.proved)std::cout<<",\"coefficients\":["<<p.coef[0]<<','<<p.coef[1]<<','<<p.coef[2]<<"],\"loglik_lower\":"<<p.loglik_lower<<",\"loglik_upper\":"<<p.loglik_upper;
  std::cout<<"}\n";return p.proved?0:1;
}
