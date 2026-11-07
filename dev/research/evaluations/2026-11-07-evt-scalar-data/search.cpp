// Independent numerical check of the monotonicity identity; no fitter calls.
#include <algorithm>
#include <cmath>
#include <iostream>
#include <random>
#include <vector>
int main(){std::mt19937_64 rng(11630);std::uniform_real_distribution<double> U(0,1);int checks=0;
for(int trial=0;trial<10000;++trial){int n=3+rng()%18;double a=std::exp(-2+5*U(rng));std::vector<double>g{0};for(int i=1;i<n;++i)g.push_back(std::exp(-10+20*U(rng)));
double e=std::exp(-10+20*U(rng));
auto E=[&](double p){double A=0,B=0;for(double v:g){double r=e/(v+e);A+=std::pow(r,p+1);B+=std::pow(r,p);}return A/B;};
double derivative=-(a+1)*E(a+1)+a*E(a)+E(1);
if(derivative>1e-12*(a+1)){std::cout<<"FAIL trial "<<trial<<"\n";return 1;}++checks;}
std::cout<<"PASS: "<<checks<<" seed-11630 monotonicity identities (floating comparison allowance 1e-12*(lambda+1)).\n";}
