#include <cmath>
#include <cstdio>
#include <random>
int main() {
  std::mt19937_64 rng(11620261103ULL);
  const double shapes[]={0.15,0.35,1,3,10,50},rates[]={0.25,1,4};
  const int sizes[]={10,30,80};
  for(int i=0;i<600;++i) {
    const int n=sizes[(i/18)%3];
    std::gamma_distribution<double> d(shapes[i%6],1/rates[(i/6)%3]);
    std::printf("%d",i);
    for(int j=0;j<n;++j) std::printf(",%.0f",std::floor(d(rng)));
    std::printf("\n");
  }
}
