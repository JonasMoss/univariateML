#include "cppunivariate/distributions/ged/dpqr.hpp"
#include <array>
#include <cmath>
#include <ctime>
#include <cstdlib>
#include <iomanip>
#include <iostream>

int main(int argc,char** argv) {
    const int selected=argc>1?std::atoi(argv[1]):-1;
    int row=0;
    constexpr std::array<std::array<double,2>,4> affine{{
        {{0.,1.}}, {{.3,1.7}}, {{-1e100,1e100}}, {{1e-100,1e-100}}
    }};
    volatile double result=0;
    std::cout << std::setprecision(9);
    for(double nu:{1.,2.,5.,50.,1e3,1e6}) {
        for(const auto &pair:affine) {
            for(bool log:{false,true})for(bool lower:{false,true}) {
                if(row++!=selected && selected>=0)continue;
                std::array<double,1998> probabilities{};
                for(int i=1;i<1000;++i) {
                    probabilities[2*(i-1)]=log ? -i/2. : i/1000.;
                    probabilities[2*(i-1)+1]=log ? -10000.*i/999.
                                                   : std::pow(10.,-300.*i/999.);
                }
                const auto q=[&](double probability) {
                    return cppunivariate::distributions::qged(
                        probability,pair[0],pair[1],nu,lower,log);
                };
                for(double probability:probabilities)result=q(probability);
                const auto start=std::clock();
                for(int repeat=0;repeat<20;++repeat)
                    for(double probability:probabilities)result=q(probability);
                const double us=1e6*(std::clock()-start)/CLOCKS_PER_SEC/39960;
                std::cout << nu << ',' << pair[0] << ',' << pair[1] << ','
                          << int(log) << ',' << int(lower) << ',' << us << '\n' << std::flush;
            }
        }
    }
}
