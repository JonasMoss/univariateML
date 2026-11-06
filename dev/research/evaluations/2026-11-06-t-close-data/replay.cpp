#include "cppunivariate/distributions/lst/certificate.hpp"
#include "cppunivariate/special/special.hpp"
#include <iostream>
#include <iomanip>
#include <chrono>
#include <cstdlib>
#include <fstream>
int main(int argc,char** argv){const int budget=argc>1?std::atoi(argv[1]):4096;std::cout<<std::setprecision(17);std::size_t n;
 while(std::cin>>n){std::vector<double>x(n);for(double& v:x)std::cin>>v;
 if(n==0){for(int i=0;i<4000;++i)x.push_back(cppunivariate::special::qt((i+.5)/4000,300,true,false));
 std::ofstream dump("build/scratch/t-close-quantile.txt");dump<<std::setprecision(17);for(double z:x)dump<<z<<"\n";}
 for(double floor:{1.,2.}){auto start=std::chrono::steady_clock::now();auto r=cppunivariate::t_certificate({x.data(),x.size()},floor,budget);
 std::cout<<int(r.status)<<" "<<r.profiles<<" "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<" "<<r.mu<<" "<<r.scale<<" "<<r.nu;
 for(int j=0;j<3;++j)std::cout<<" "<<r.coefficient_lower[j]<<" "<<r.coefficient_upper[j];
 std::cout<<" "<<r.unresolved_shape[0]<<" "<<r.unresolved_shape[1]<<std::endl;}}
}
