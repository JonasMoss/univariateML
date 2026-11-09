#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <fstream>
#include <iostream>
#include <iomanip>
int main(){std::ifstream input("build/scratch/bnbinom-allfree/counts.txt");std::map<int,double>counts;int x;while(input>>x)counts[x]++;
  cppunivariate::bnbinom_detail::Data d;for(auto kv:counts){d.x.push_back(kv.first);d.count.push_back(kv.second);d.n+=kv.second;}
  std::vector<double> p{1.0672234787479615,2.1419791546447171,5.3647338351087397};
  std::array<std::array<double,3>,3> h{};
  const double r=p[0],a=p[1],b=p[2],ar=cppunivariate::special::trigamma(a+r),ab=cppunivariate::special::trigamma(a+b);
  h[0][0]=ar-cppunivariate::special::trigamma(r);h[1][1]=ar+ab-cppunivariate::special::trigamma(a);h[2][2]=ab-cppunivariate::special::trigamma(b);h[0][1]=ar;h[1][2]=ab;
  for(auto kv:counts){double w=kv.second/d.n,t=cppunivariate::special::trigamma(a+r+b+kv.first);for(int i=0;i<3;++i)for(int j=i;j<3;++j)h[i][j]-=w*t;h[0][0]+=w*cppunivariate::special::trigamma(r+kv.first);h[2][2]+=w*cppunivariate::special::trigamma(b+kv.first);}
  for(int i=0;i<3;++i)for(int j=0;j<i;++j)h[i][j]=h[j][i];
  const double det=h[0][0]*(h[1][1]*h[2][2]-h[1][2]*h[2][1])-h[0][1]*(h[1][0]*h[2][2]-h[1][2]*h[2][0])+h[0][2]*(h[1][0]*h[2][1]-h[1][1]*h[2][0]);
  std::cout<<std::setprecision(17)<<"{\"determinant\":"<<det<<",\"relative_score_margin_floors\":[";
  for(int i=0;i<3;++i){double floor=0;for(int j=0;j<3;++j){const int u=(j+1)%3,v=(j+2)%3,s=(i+1)%3,t=(i+2)%3;double inv=(h[u][s]*h[v][t]-h[u][t]*h[v][s])/det;floor+=std::abs(inv)*cppunivariate::bnbinom_detail::scalar_score(d,p,j).margin;}if(i)std::cout<<',';std::cout<<floor/p[i];}std::cout<<"]}\n";
}
