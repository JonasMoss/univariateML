#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <fstream>
#include <iostream>
#include <iomanip>
#include <chrono>
int main(int argc,char **argv) {
 std::vector<int>x;
 if(argc>1){std::ifstream f(argv[1]);int v;while(f>>v)x.push_back(v);}
 else x={0,0,1,2,3,6,12,24};
 for(double a:{.5,1.,2.,2.1419791546447171,5.,20.}) {
 auto start=std::chrono::steady_clock::now();
 auto f=cppunivariate::bnbinom_mle({x.data(),x.size()},1e-8,600,NAN,a,NAN);
 std::cout<<std::setprecision(17)<<a<<" status "<<static_cast<int>(f.status)<<" p "<<f.size<<','<<f.alpha<<','<<f.beta<<" ll "<<f.logLik<<" iterations "<<f.iterations<<" seconds "<<std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count()<<" witnesses "<<f.stop_detail.candidates.size()<<std::endl;
 for(const auto &c:f.stop_detail.candidates)std::cout<<c.coef[0]<<','<<c.coef[1]<<','<<c.coef[2]<<' '<<c.loglik_lower<<' '<<c.loglik_upper<<std::endl;
 }
}
