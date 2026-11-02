#include SLASH_HEADER
#include <random>
#include <vector>
#include <cstdio>
#include <chrono>
int main(){std::mt19937 gen(116);std::normal_distribution<double> z;std::uniform_real_distribution<double> u;std::vector<double>x;for(int i=0;i<500;++i)x.push_back(z(gen)/u(gen));const auto start=std::chrono::steady_clock::now();auto f=cppunivariate::slash_mle(cppunivariate::span{x.data(),x.size()});std::printf("status=%d mu=%.17g sigma=%.17g logLik=%.17g seconds=%.6f\n",int(f.status),f.mu,f.sigma,f.logLik,std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count());}
