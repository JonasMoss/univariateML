#include SLASH_HEADER
#include <random>
#include <vector>
#include <cmath>
#include <cstdio>
int main() {
  std::mt19937 gen(1162026);
  std::uniform_real_distribution<double> unif(0,1);
  std::normal_distribution<double> normal;
  for(int i=0;i<600;++i) {
    std::vector<double> x;
    if(i%30==0) {
      for(int j=0;j<10;++j)x.push_back(0);
      x.push_back(-1);
      for(int j=0;j<9;++j)x.push_back(9+((i/30)%3));
    } else {
      const int n=7+i%14;
      for(int j=0;j<n;++j) {
        double v=normal(gen)/(0.1+0.9*unif(gen));
        if(i%11==0)v=std::round(v);
        x.push_back(v);
      }
      if(i%13==0)for(int j=0;j<(n-1)/2;++j)x[j]=0;
    }
    const auto f=cppunivariate::slash_mle(cppunivariate::span{x.data(),x.size()});
    std::printf("%d,%zu,%d,%.17g,%.17g,%.17g\n",i,x.size(),static_cast<int>(f.status),f.mu,f.sigma,f.logLik);
  }
}
